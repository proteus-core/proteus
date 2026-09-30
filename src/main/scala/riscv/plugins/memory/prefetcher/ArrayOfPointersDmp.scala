package riscv.plugins.memory.prefetcher

import riscv._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/** A data memory-dependent prefetcher that looks for an array of pointers dereferencing pattern,
  * modeled after the Intel DMP described in https://gofetch.fail/files/peek-a-walk.pdf. It stores
  * the past `loadHistorySize` loads, comparing current loaded addresses with values in the load
  * history. Once it has reached sufficient confidence (`activationConfidence`), it will activate
  * and prefetch and dereference pointers (where the pointers must pass the `pointerHeuristic` to be
  * dereferenced) that are `prefetchDistance` elements ahead in the pattern, relying on a prefetch
  * queue of size `prefetchQueueSize`. `alignmentBits` determines how many of the upper bits of the
  * strided target address must be equal to the original address, and `pcCompareBits` determines how
  * many bits of the PC to store and compare for pattern detection.
  */
class ArrayOfPointersDmp(
    loadHistorySize: Int,
    prefetchQueueSize: Int,
    activationConfidence: Int,
    maxConfidence: Int,
    prefetchDistance: Int,
    alignmentBits: Int,
    pcCompareBits: Int,
    strideDetectionLeniency: Int,
    pointerHeuristic: (UInt, UInt) => Bool
)(implicit config: Config)
    extends Plugin[Pipeline]
    with PrefetchService {

  private case class LoadHistoryEntry() extends Bundle {
    val address: UInt = UInt(config.isa.xlen bits)
    val pc: UInt = UInt(pcCompareBits bits)
    val value: UInt = UInt(config.isa.xlen bits)
  }
  private var loadHistory: Vec[LoadHistoryEntry] = null

  private case class PrefetchQueueEntry() extends Bundle {
    val isValid: Bool = Bool()
    val targetAddress: UInt = UInt(config.isa.xlen bits)
    val isTargetAddressPrefetched: Bool = Bool()
    val value: UInt = UInt(config.isa.xlen bits)
    val isValueValid: Bool = Bool()
  }
  private var prefetchQueue: Vec[PrefetchQueueEntry] = null
  private var prefetchQueueNext: Vec[PrefetchQueueEntry] = null

  private def registerLoad(entry: LoadHistoryEntry): Unit = {
    for (i <- loadHistorySize - 1 downto 1) {
      loadHistory(i) := loadHistory(i - 1)
    }
    loadHistory(0) := entry
  }

  private def addToPrefetchQueue(entry: PrefetchQueueEntry): Unit = {
    for (i <- prefetchQueueSize - 1 downto 1) {
      prefetchQueue(i) := prefetchQueueNext(i - 1)
    }
    prefetchQueue(0) := entry
  }

  private object PrefetchState extends SpinalEnum {
    val INACTIVE, CALCULATE_STRIDE, ACTIVE = newElement()
  }
  // can only learn one pattern at a time, can be modified to a lookup table for learning multiple patterns
  private var prefetchState: SpinalEnumCraft[PrefetchState.type] = null
  private var confidence: UInt = null
  private var baseAddress: UInt = null
  private var learnedPc: UInt = null
  private var stride: SInt = null

  private var storedCacheLineAddress: UInt = null
  private var storedCacheLineValue: UInt = null

  private var busy: Bool = null
  private var isReceivingLoadResponse: Bool = null
  private var responseAddress: UInt = null
  private var responsePc: UInt = null
  private var responseData: UInt = null

  private val wordsInCacheLine = config.memBusWidth / config.isa.xlen
  private val byteIndexBits = log2Up(config.isa.xlen / 8)
  private val wordIndexBits = log2Up(wordsInCacheLine)

  // signals to expose to noninterference security script
  private var noninterferenceSignals: Bits = null
  private var noninterferenceSignal1: Bits = null
  private var noninterferenceSignal2: Bits = null
  private var noninterferenceSignal3: Bits = null
  private var noninterferenceSignal4: Bits = null
  private var noninterferenceSignal5: Bits = null

  private def getCacheLineBits(address: UInt): UInt = {
    address(config.isa.xlen - 1 downto byteIndexBits + wordIndexBits)
  }

  private def getWordIndexBits(address: UInt): UInt = {
    address(byteIndexBits + wordIndexBits - 1 downto byteIndexBits)
  }

  private def getWordFromCacheLine(address: UInt, data: UInt): UInt = {
    data(getWordIndexBits(address) << log2Up(config.isa.xlen), config.isa.xlen bits)
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      loadHistory = Vec
        .fill(loadHistorySize)(RegInit(LoadHistoryEntry().getZero))
        .simPublic
        .setName("dataPrefetcher_loadHistory")
      prefetchQueue = Vec
        .fill(prefetchQueueSize)(RegInit(PrefetchQueueEntry().getZero))
        .simPublic
        .setName("dataPrefetcher_prefetchQueue")
      prefetchQueueNext = Vec
        .fill(prefetchQueueSize)(PrefetchQueueEntry())
        .simPublic()
        .setName("dataPrefetcher_prefetchQueueNext")
      for (i <- 0 until prefetchQueueSize) {
        prefetchQueueNext(i) := prefetchQueue(i)
        prefetchQueue(i) := prefetchQueueNext(i)
      }

      prefetchState = Reg(PrefetchState())
        .init(PrefetchState.INACTIVE)
        .simPublic
        .setName("dataPrefetcher_prefetchState")
      confidence = RegInit(UInt(log2Up(maxConfidence + 1) bits).getZero).simPublic
        .setName("dataPrefetcher_confidence")
      baseAddress =
        RegInit(UInt(config.isa.xlen bits).getZero).simPublic.setName("dataPrefetcher_baseAddress")
      learnedPc =
        RegInit(UInt(pcCompareBits bits).getZero).simPublic.setName("dataPrefetcher_learnedPc")
      stride =
        RegInit(SInt(config.isa.xlen bits).getZero).simPublic.setName("dataPrefetcher_stride")

      storedCacheLineAddress = RegInit(UInt(config.isa.xlen bits).getZero).simPublic
        .setName("dataPrefetcher_storedCacheLineAddress")
      storedCacheLineValue = RegInit(UInt(config.memBusWidth bits).getZero).simPublic
        .setName("dataPrefetcher_storedCacheLineValue")

      busy = Bool()
      busy := False
      isReceivingLoadResponse = Bool()
      isReceivingLoadResponse := False
      responseAddress = UInt(config.isa.xlen bits)
      responseAddress := 0
      responsePc = UInt(config.isa.xlen bits)
      responsePc := 0
      responseData = UInt(config.memBusWidth bits)
      responseData := 0

      noninterferenceSignal1 = Bits(config.isa.xlen + config.isa.xlen + pcCompareBits bits)
      noninterferenceSignal1 := 0
      noninterferenceSignal2 = Bits(config.isa.xlen + pcCompareBits bits)
      noninterferenceSignal2 := 0
      noninterferenceSignal3 = Bits(config.isa.xlen bits)
      noninterferenceSignal3 := 0
      noninterferenceSignal4 = Bits(config.isa.xlen bits)
      noninterferenceSignal4 := 0
      noninterferenceSignal5 = Bits(config.isa.xlen * prefetchQueueSize bits)
      noninterferenceSignal5 := 0
      noninterferenceSignals = (noninterferenceSignal1 ## noninterferenceSignal2 ##
        noninterferenceSignal3 ## noninterferenceSignal4 ## noninterferenceSignal5).simPublic
        .setName("dataPrefetcher_noninterferenceSignals")
    }
  }

  override def build(): Unit = {
    pipeline plug new Area {
      // handle a load response from cache hits and misses:
      // 1. add it to the load history,
      // 2. look for the array of pointers dereferencing pattern, and based on this:
      // 3. update state: prefetchState, baseAddress, stride, confidence, learnedPc
      when(isReceivingLoadResponse) {
        val loadedValue = getWordFromCacheLine(responseAddress, responseData)

        // dmp analyzes the address, pc and value => leak
        noninterferenceSignal1 := responseAddress ## loadedValue ## responsePc(
          pcCompareBits - 1 downto 0
        )

        // 1. add load to the load history
        val currentLoad: LoadHistoryEntry = LoadHistoryEntry()
        currentLoad.address := responseAddress
        currentLoad.pc := responsePc(pcCompareBits - 1 downto 0)
        currentLoad.value := loadedValue
        registerLoad(currentLoad)

        // 2. check if the current load address is a load value in the load history
        val (valueFound, loadHistoryIndex) =
          loadHistory.sFindFirst(load => load.value === responseAddress)

        // 3. update the prefetcher state
        when(valueFound) {
          when(prefetchState === PrefetchState.INACTIVE) {
            // stage 1: remember base address and pc, use it later to calculate stride
            baseAddress := loadHistory(loadHistoryIndex).address
            prefetchState := PrefetchState.CALCULATE_STRIDE
            learnedPc := loadHistory(loadHistoryIndex).pc
          } elsewhen (prefetchState === PrefetchState.CALCULATE_STRIDE) {
            // stage 2: calculate stride
            val newStride = (loadHistory(loadHistoryIndex).address.asSInt - baseAddress.asSInt)
            when(newStride =/= 0 && learnedPc === loadHistory(loadHistoryIndex).pc) {
              stride := newStride
              baseAddress := loadHistory(loadHistoryIndex).address
              confidence := 0
              prefetchState := PrefetchState.ACTIVE
            } otherwise {
              prefetchState := PrefetchState.INACTIVE
              baseAddress := 0
              learnedPc := 0
            }
          } otherwise {
            // stage 3: check stride and update confidence
            // leniency for if, e.g. element 3 is loaded before 2 due to out of order scheduling:
            // update baseAddress to element 3 and ignore the later load for element 2
            val isStridedAddress = Bool()
            isStridedAddress := False
            val ignoreLoad = Bool()
            ignoreLoad := False
            val pcComparison = learnedPc === loadHistory(loadHistoryIndex).pc
            for (i <- -1 * strideDetectionLeniency to strideDetectionLeniency + 1) {
              when(
                pcComparison && (baseAddress.asSInt + (i * stride).resize(
                  config.isa.xlen
                )).asUInt === loadHistory(loadHistoryIndex).address
              ) {
                if (i <= 0) {
                  // ignore if it is a replay (i=0) or older element in the pattern (i < 0) due to reordering
                  ignoreLoad := True
                } else {
                  // update baseAddress if it is ahead in the pattern
                  isStridedAddress := True
                }
              }
            }
            when(isStridedAddress) {
              baseAddress := loadHistory(loadHistoryIndex).address
              when(confidence =/= maxConfidence) {
                confidence := confidence + 1
              }
            } elsewhen (!ignoreLoad) {
              when(confidence === 0) {
                // elsewhen / otherwise: a pointer dereference is detected that does not match the learned pattern
                prefetchState := PrefetchState.INACTIVE
                baseAddress := 0
                stride := 0
                learnedPc := 0
              } otherwise {
                confidence := confidence - 1
              }
            }
          }
        }
      }
    }
  }

  override def notifyLoadRequest(address: UInt, pc: UInt): Unit = {
    // dmp analyzes the address and pc => leak
    noninterferenceSignal2 := address ## pc(pcCompareBits - 1 downto 0)

    // when the DMP is active, it triggers upon loading an element of the array:
    // 1. it prefetches an element ahead in the array,
    // 2. it read the value, and
    // 3. dereferences the value
    when(
      prefetchState === PrefetchState.ACTIVE && confidence >= activationConfidence && learnedPc === pc(
        pcCompareBits - 1 downto 0
      )
    ) {
      val prefetchTarget =
        (address.asSInt + (prefetchDistance * stride).resize(config.isa.xlen)).asUInt
      when(prefetchTarget.takeHigh(alignmentBits) === address.takeHigh(alignmentBits)) {
        val newPrefetchEntry = PrefetchQueueEntry()
        val addEntryToQueue = Bool()
        addEntryToQueue := True
        newPrefetchEntry.isValid := True
        newPrefetchEntry.targetAddress := prefetchTarget
        when(
          getCacheLineBits(storedCacheLineAddress) === getCacheLineBits(prefetchTarget)
        ) {
          val word = getWordFromCacheLine(prefetchTarget, storedCacheLineValue)
          // dmp analyzes the word: leak
          noninterferenceSignal3 := word.asBits
          newPrefetchEntry.isTargetAddressPrefetched := True
          newPrefetchEntry.value := word
          newPrefetchEntry.isValueValid := True
          when(!pointerHeuristic(prefetchTarget, word)) {
            addEntryToQueue := False
          }
        } otherwise {
          newPrefetchEntry.isTargetAddressPrefetched := False
          newPrefetchEntry.value := 0
          newPrefetchEntry.isValueValid := False
        }
        when(addEntryToQueue) {
          addToPrefetchQueue(newPrefetchEntry)
        }
      }
    }
  }

  override def notifyLoadResponse(address: UInt, pc: UInt, data: UInt, cacheHit: Boolean): Unit = {
    isReceivingLoadResponse := True
    responseAddress := address
    responsePc := pc
    responseData := data
  }

  override def notifyPrefetchResponse(address: UInt, data: UInt, id: UInt): Unit = {
    // dmp analyzes the address => leak
    noninterferenceSignal4 := address.asBits

    busy := True

    for (i <- 0 until prefetchQueueSize) {
      val entry = prefetchQueue(i)
      when(
        entry.isValid && entry.isTargetAddressPrefetched && !entry.isValueValid && getCacheLineBits(
          entry.targetAddress
        ) === getCacheLineBits(address)
      ) {
        storedCacheLineAddress := address
        storedCacheLineValue := data
        val word = getWordFromCacheLine(entry.targetAddress, data)
        // dmp analyzes the word => leak
        noninterferenceSignal5(i * config.isa.xlen, config.isa.xlen bits) := word.asBits
        when(pointerHeuristic(entry.targetAddress, word)) {
          prefetchQueueNext(i).value := word
          prefetchQueueNext(i).isValueValid := True
        } otherwise {
          prefetchQueueNext(i).isValid := False
        }
      }
    }
  }

  override def getNextPrefetchTarget(id: UInt): UInt = {
    // check from old to new, for each entry check first the target, then the value to prefetch
    val target = UInt(config.isa.xlen bits)
    target := 0
    var alreadyFound: Bool = False
    for (i <- prefetchQueueSize - 1 downto 0) {
      val entry = prefetchQueue(i)
      val validPrefetchTarget = entry.isValid && !entry.isTargetAddressPrefetched
      val validPrefetchValue = entry.isValid && entry.isValueValid

      when(!alreadyFound) {
        when(validPrefetchTarget) {
          target := entry.targetAddress
          prefetchQueueNext(i).isTargetAddressPrefetched := True
        } elsewhen (validPrefetchValue) {
          target := entry.value
          prefetchQueueNext(i).isValid := False
        }
      }

      alreadyFound = validPrefetchTarget || validPrefetchValue || alreadyFound
    }
    target
  }

  override def hasPrefetchTarget: Bool = {
    !busy && prefetchQueue.sExist(entry =>
      entry.isValid && (!entry.isTargetAddressPrefetched || entry.isValueValid)
    )
  }
}
