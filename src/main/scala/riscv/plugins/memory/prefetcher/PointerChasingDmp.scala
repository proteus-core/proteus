package riscv.plugins.memory.prefetcher

import riscv._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/** A pointer-chasing data memory-dependent prefetcher modeled after the DMP described in
  * https://dl.acm.org/doi/10.1145/605397.605427. It scans all blocks loading into the cache for
  * pointers, applying the provided pointer heuristic. The `maxRecursionDepth` specifies the maximum
  * depth of which the DMP can recursively analyze prefetched data. A `maxRecursionDepth` of 0
  * disables any recursive behavior, meaning the DMP will only analyze data requested directly by
  * the program. A negative `maxRecursionDepth` enables infinite recursive prefetching.
  * `nextLinePrefetches` determines the width of prefetches: whether only the target should be
  * prefetched, or also the `nextLinePrefetches` cache lines following the target.
  * `prefetchQueueSize` determines the size of the prefetch queue that stores the prefetch targets.
  */
class PointerChasingDmp(
    pointerHeuristic: (UInt, UInt) => Bool,
    maxRecursionDepth: Int,
    nextLinePrefetches: Int,
    prefetchQueueSize: Int
)(implicit
    config: Config
) extends Plugin[Pipeline]
    with PrefetchService {

  private val depthBits = if (maxRecursionDepth > 0) log2Up(maxRecursionDepth + 2) else 1
  private val wordsInCacheLine = config.memBusWidth / config.isa.xlen
  private val byteIndexBits = log2Up(config.isa.xlen / 8)
  private val wordIndexBits = log2Up(wordsInCacheLine)

  // queue with the next prefetches to be issued
  private case class PrefetchQueueEntry() extends Bundle {
    val isValid: Bool = Bool()
    val targetAddress: UInt = UInt(config.isa.xlen bits)
    val depth: UInt = UInt(depthBits bits)
    val nextLineCount: UInt =
      if (nextLinePrefetches > 0) UInt(log2Up(nextLinePrefetches + 1) bits) else UInt(1 bit)
  }
  private var prefetchQueue: Vec[PrefetchQueueEntry] = null

  // track the depths of pending prefetches
  private var prefetchDepths: Vec[UInt] = null

  private var isUpdatingState: Bool = null

  // signals to expose to noninterference security script
  private var noninterferenceSignals: Bits = null

  override def setup(): Unit = {
    pipeline plug new Area {
      prefetchQueue = Vec
        .fill(prefetchQueueSize)(RegInit(PrefetchQueueEntry().getZero))
        .simPublic
        .setName("dataPrefetcher_prefetchQueue")

      isUpdatingState = Bool()
      isUpdatingState := False

      if (maxRecursionDepth != 0) {
        prefetchDepths = Vec(
          RegInit(UInt(depthBits bits).getZero),
          UInt(config.externalDBusConfig.idWidth bits).maxValue.intValue + 1
        )
      }

      noninterferenceSignals = Bits(config.isa.xlen + config.memBusWidth bits).simPublic
        .setName("dataPrefetcher_noninterferenceSignals")
      noninterferenceSignals := 0
    }
  }

  def updateState(address: UInt, data: UInt, depth: UInt): Unit = {
    // dmp analyzes the address and data => leak
    noninterferenceSignals := address ## data

    // rotate the cache line so that the word at `address` is at position 0
    val rotatedData = data.rotateRight(
      address(byteIndexBits + wordIndexBits - 1 downto byteIndexBits) << log2Up(config.isa.xlen)
    )

    // extract each word and apply the pointer heuristic
    val candidates = rotatedData.subdivideIn(config.isa.xlen bits)
    val isPointer = candidates.map(candidate => pointerHeuristic(address, candidate))
    val validCount = isPointer.sCount(entry => entry)

    val newPrefetchQueue = Vec(PrefetchQueueEntry(), prefetchQueueSize)
    newPrefetchQueue := (prefetchQueue.asBits |<< (validCount * PrefetchQueueEntry().getBitsWidth))
      .as(Vec(PrefetchQueueEntry(), prefetchQueueSize))

    when(isPointer.asBits =/= 0) {
      isUpdatingState := True
      var queueIdx: UInt = U(0, log2Up(prefetchQueueSize) bits)
      for (i <- wordsInCacheLine - 1 downto 0) {
        when(isPointer(i)) {
          val newEntry: PrefetchQueueEntry = PrefetchQueueEntry()
          newEntry.isValid := True
          newEntry.targetAddress := candidates(i)
          newEntry.depth := depth
          newEntry.nextLineCount := 0
          newPrefetchQueue(queueIdx) := newEntry
        }
        // if isPointer(i) queueIdx += 1
        queueIdx = queueIdx + isPointer(i).asUInt.resize(queueIdx.getWidth)
      }

      prefetchQueue := newPrefetchQueue
    }
  }

  override def notifyLoadRequest(address: UInt, pc: UInt): Unit = {}

  override def notifyLoadResponse(address: UInt, pc: UInt, data: UInt, cacheHit: Boolean): Unit = {
    if (!cacheHit) {
      updateState(address, data, U(0, depthBits bits))
    }
  }

  override def notifyPrefetchResponse(address: UInt, data: UInt, id: UInt): Unit = {
    if (maxRecursionDepth != 0) {
      val depth = prefetchDepths(id)
      val maxRecursionCheck = if (maxRecursionDepth > 0) depth <= maxRecursionDepth else True

      when(maxRecursionCheck) {
        updateState(address, data, depth)
      }
    }
  }

  override def getNextPrefetchTarget(id: UInt): UInt = {
    val nextTargetIdx = UInt(log2Up(prefetchQueueSize) bits)
    nextTargetIdx := 0
    var foundValidTarget: Bool = False
    var currentBestNextLineCount: UInt = U(0, widthOf(prefetchQueue(0).nextLineCount) bits)
    var currentBestDepth: UInt = U(0, depthBits bits)

    // get the next entry: give priority to targets with lower depth and lower nextLineCount
    for (i <- prefetchQueueSize - 1 downto 0) {
      val entry = prefetchQueue(i)
      val isBetterThanCurrent = entry.nextLineCount < currentBestNextLineCount ||
        (entry.nextLineCount === currentBestNextLineCount && entry.depth < currentBestDepth)
      val isNewBest = entry.isValid && (!foundValidTarget || isBetterThanCurrent)
      when(isNewBest) {
        nextTargetIdx := i
      }
      currentBestNextLineCount = Mux(isNewBest, entry.nextLineCount, currentBestNextLineCount)
      currentBestDepth = Mux(isNewBest, entry.depth, currentBestDepth)
      foundValidTarget = foundValidTarget || entry.isValid
    }

    // update entry in the prefetch queue
    val nextEntry = prefetchQueue(nextTargetIdx)
    if (maxRecursionDepth != 0) {
      prefetchDepths(id) := (if (maxRecursionDepth > 0) nextEntry.depth + 1 else U(1))
    }
    when(nextEntry.nextLineCount =/= nextLinePrefetches) {
      nextEntry.nextLineCount := nextEntry.nextLineCount + 1
    } otherwise {
      nextEntry.isValid := False
    }

    // return the target
    nextEntry.targetAddress + (nextEntry.nextLineCount << byteIndexBits + wordIndexBits).resize(
      config.isa.xlen
    )
  }

  override def hasPrefetchTarget: Bool = {
    !isUpdatingState && prefetchQueue.sExist(entry => entry.isValid)
  }
}
