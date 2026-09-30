package riscv.plugins.memory.prefetcher

import riscv._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/** A data memory-dependent prefetcher that looks for indirect array access patterns, i.e., patterns
  * of the form `A[B[i]]`. It is modeled after the IMP DMP described in
  * https://ieeexplore.ieee.org/document/7856597.
  *
  * The DMP first registers loads in the prefetch table of size `prefetchTableSize`, detecting
  * strided array access patterns, comparing on `pcCompareBits` of the PC. Once a stride pattern has
  * been detected (reaching a confidence hit count of `strideActivationHitCount`), it allocates an
  * entry in the indirect pattern detector table of size `indirectPatternDetectorTableSize`. The
  * indirect pattern detector shifts candidate index values by the shifts in the `shiftValues`
  * array, calculating possible base addresses of the indirect array and storing them in an array of
  * size `baseAddressArrayLength`. Once the indirect pattern has been detected, it will verify the
  * pattern, increasing the indirect hit count. When an indirect hit count of
  * `indirectActivationHitCount` has been reached, the DMP activates and issues prefetches to a
  * prefetch queue of size `prefetchQueueSize`: it prefetches an entry ahead in the stride array
  * (gradually increasing the prefetch distance up to `maxPrefetchDistance`), adds the returned
  * value (after shifting it with the selected shift) to the calculated indirect base address, and
  * prefetches the result if it passes the `pointerHeuristic`.
  */
class IndirectArrayPatternDmp(
    prefetchTableSize: Int,
    indirectPatternDetectorTableSize: Int,
    pcCompareBits: Int,
    shiftValues: Seq[Int],
    baseAddressArrayLength: Int,
    prefetchQueueSize: Int,
    strideActivationHitCount: Int,
    indirectActivationHitCount: Int,
    maxHitCount: Int,
    maxPrefetchDistance: Int,
    strideDetectionLeniency: Int,
    indirectVerificationLeniency: Int,
    pointerHeuristic: (UInt, UInt) => Bool // True
)(implicit config: Config)
    extends Plugin[Pipeline]
    with PrefetchService {

  private case class PrefetchTableEntry() extends Bundle {
    val valid: Bool = Bool()
    val pc: UInt = UInt(pcCompareBits bits)
    val currentStrideAddress: UInt = UInt(config.isa.xlen bits)
    val currentValue: UInt = UInt(config.isa.xlen bits)
    val strideHitCount: UInt = UInt(log2Up(strideActivationHitCount + 1) bits)
    val active: Bool = Bool()
    val isVerifyingIndirectAccess: Bool = Bool()
    val missedIndirectAccessCount: UInt = UInt(log2Up(indirectVerificationLeniency + 1) bits)
    val indirectBaseAddress: UInt = UInt(config.isa.xlen bits)
    val shiftIndex: UInt = UInt(log2Up(shiftValues.length) bits)
    val indirectHitCount: UInt = UInt(log2Up(maxHitCount + 1) bits)
    val prefetchDistance: UInt = UInt(log2Up(maxPrefetchDistance + 1) bits)
  }
  private var prefetchTable: Vec[PrefetchTableEntry] = null

  private object IndirectPatternDetectorState extends SpinalEnum {
    val INVALID, CALCULATE_BASE_ADDRESSES, FIND_MATCH = newElement()
  }
  private case class IndirectPatternDetectorEntry() extends Bundle {
    val state: SpinalEnumCraft[IndirectPatternDetectorState.type] = IndirectPatternDetectorState()
    val prefetchTableIndex: UInt = UInt(log2Up(prefetchTableSize) bits)
    val index1: UInt = UInt(config.isa.xlen bits)
    val index2: UInt = UInt(config.isa.xlen bits)
    val baseAddresses: Vec[Vec[UInt]] =
      Vec.fill(shiftValues.length)(Vec.fill(baseAddressArrayLength)(UInt(config.isa.xlen bits)))
    val missCount: UInt = UInt(log2Up(baseAddressArrayLength + 1) bits)
    val hasComparedCacheMiss: Bool = Bool()
  }
  private var indirectPatternDetectorTable: Vec[IndirectPatternDetectorEntry] = null
  private var indirectPatternDetectorAllocationPointer: UInt = null

  private case class PrefetchQueueEntry() extends Bundle {
    val isValid: Bool = Bool()
    val targetAddress: UInt = UInt(config.isa.xlen bits)
    val isTargetAddressPrefetched: Bool = Bool()
    val shiftIndex: UInt = UInt(log2Up(shiftValues.length) bits)
    val indirectBaseAddress: UInt = UInt(config.isa.xlen bits)
    val indirectAddress: UInt = UInt(config.isa.xlen bits)
    val isIndirectAddressValid: Bool = Bool()
  }
  private var prefetchQueue: Vec[PrefetchQueueEntry] = null
  private var prefetchQueueNext: Vec[PrefetchQueueEntry] = null

  private def addToPrefetchQueue(entry: PrefetchQueueEntry): Unit = {
    for (i <- prefetchQueueSize - 1 downto 1) {
      prefetchQueue(i) := prefetchQueueNext(i - 1)
    }
    prefetchQueue(0) := entry
  }

  private var storedCacheLineAddress: UInt = null
  private var storedCacheLineValue: UInt = null

  private var busy: Bool = null
  private var isReceivingLoadResponse: Bool = null
  private var isResponseCacheMiss: Bool = null
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

  private def shiftValue(value: UInt, shift: Int): UInt = {
    val shiftedValue = if (shift >= 0) value << shift else value >> -shift
    shiftedValue.resize(config.isa.xlen)
  }

  private def calculateIndirectAddress(baseAddress: UInt, shiftIndex: UInt, value: UInt): UInt = {
    val shiftedValue = UInt(config.isa.xlen bits)
    shiftedValue := 0
    for ((shift, i) <- shiftValues.zipWithIndex) {
      when(shiftIndex === i) {
        shiftedValue := shiftValue(value, shift)
      }
    }
    baseAddress + shiftedValue
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      prefetchTable = Vec
        .fill(prefetchTableSize)(RegInit(PrefetchTableEntry().getZero))
        .simPublic
        .setName("dataPrefetcher_prefetchTable")
      indirectPatternDetectorTable = Vec
        .fill(indirectPatternDetectorTableSize)(RegInit(IndirectPatternDetectorEntry().getZero))
        .simPublic
        .setName("dataPrefetcher_indirectPatternDetectorTable")
      indirectPatternDetectorAllocationPointer = RegInit(
        UInt(log2Up(indirectPatternDetectorTableSize) bits).getZero
      ).simPublic.setName("dataPrefetcher_indirectPatternDetectorAllocationPointer")
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

      storedCacheLineAddress = RegInit(UInt(config.isa.xlen bits).getZero).simPublic
        .setName("dataPrefetcher_storedCacheLineAddress")
      storedCacheLineValue = RegInit(UInt(config.memBusWidth bits).getZero).simPublic
        .setName("dataPrefetcher_storedCacheLineValue")

      busy = Bool()
      busy := False
      isReceivingLoadResponse = Bool()
      isReceivingLoadResponse := False
      isResponseCacheMiss = Bool()
      isResponseCacheMiss := False
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
      // 1. look for a strided array access pattern
      // 2. when stride pattern has been detected with high enough hit count, look for the indirect pattern
      // 3. verify indirect patterns
      when(isReceivingLoadResponse) {
        val loadedValue = getWordFromCacheLine(responseAddress, responseData)
        val isAllocatingIndirectPatternDetectorEntry = Bool()
        isAllocatingIndirectPatternDetectorEntry := False

        // dmp analyzes the address, pc and value => leak
        noninterferenceSignal1 := responseAddress ## loadedValue ## responsePc(
          pcCompareBits - 1 downto 0
        )

        // check if the current pc is present in the prefetch table, indicating a possible strided array access
        val (prefetchTableEntryFound, prefetchTableIndex) = prefetchTable.sFindFirst(entry =>
          entry.pc === responsePc(pcCompareBits - 1 downto 0) && entry.valid
        )
        when(prefetchTableEntryFound) {
          val prefetchTableEntry = prefetchTable(prefetchTableIndex)
          val isLearningIndirectPattern = indirectPatternDetectorTable.sExist(entry =>
            entry.state =/= IndirectPatternDetectorState.INVALID && entry.prefetchTableIndex === prefetchTableIndex
          )
          // update existing entry in the stride table
          // check if it is a continuation of the stride access, with stride = config.isa.xlen / 8
          // leniency for if, e.g. element 3 is loaded before 2 due to out of order scheduling:
          val isStridedAddress = Bool()
          isStridedAddress := False
          val ignoreLoad = Bool()
          ignoreLoad := False
          for (i <- -1 * strideDetectionLeniency to strideDetectionLeniency + 1) {
            val lenientStrideAddress =
              if (i >= 0) prefetchTableEntry.currentStrideAddress + (i * config.isa.xlen / 8)
              else prefetchTableEntry.currentStrideAddress - (-i * config.isa.xlen / 8)
            when(lenientStrideAddress === responseAddress) {
              if (i <= 0) {
                // ignore if it is a replay (i=0) or older element in the pattern (i < 0) due to reordering
                ignoreLoad := True
              } else {
                // update the base address if it is ahead in the pattern
                isStridedAddress := True
              }
            }
          }
          when(isStridedAddress) {
            prefetchTableEntry.currentStrideAddress := responseAddress
            prefetchTableEntry.currentValue := loadedValue
            // increase hit count until activation hit count has been reached
            when(prefetchTableEntry.strideHitCount =/= strideActivationHitCount) {
              prefetchTableEntry.strideHitCount := prefetchTableEntry.strideHitCount + 1
            } otherwise {
              // stride pattern has reached required hit count: look for the indirect pattern
              when(!prefetchTableEntry.active) {
                // the indirect pattern has not yet been detected
                // check if an indirect pattern is already being learned for the stride pattern
                val (indirectPatternDetectorEntryFound, indirectPatternDetectorTableIndex) =
                  indirectPatternDetectorTable.sFindFirst(entry =>
                    entry.state =/= IndirectPatternDetectorState.INVALID &&
                      entry.prefetchTableIndex === prefetchTableIndex
                  )
                when(indirectPatternDetectorEntryFound) {
                  val indirectPatternDetectorEntry =
                    indirectPatternDetectorTable(indirectPatternDetectorTableIndex)
                  when(
                    indirectPatternDetectorEntry.state === IndirectPatternDetectorState.CALCULATE_BASE_ADDRESSES
                  ) {
                    // we just received the second index value
                    when(loadedValue =/= indirectPatternDetectorEntry.index1) {
                      indirectPatternDetectorEntry.state := IndirectPatternDetectorState.FIND_MATCH
                      indirectPatternDetectorEntry.index2 := loadedValue
                      indirectPatternDetectorEntry.hasComparedCacheMiss := False
                    } otherwise {
                      indirectPatternDetectorEntry := indirectPatternDetectorEntry.getZero
                    }
                  } elsewhen (indirectPatternDetectorEntry.state === IndirectPatternDetectorState.FIND_MATCH
                    && indirectPatternDetectorEntry.hasComparedCacheMiss) {
                    // we got a third index value without finding baseAddress: clear
                    indirectPatternDetectorEntry := indirectPatternDetectorEntry.getZero
                  }
                } otherwise {
                  // record an entry in the indirect pattern detector
                  isAllocatingIndirectPatternDetectorEntry := True
                  val newIndirectPatternDetectorEntry = IndirectPatternDetectorEntry()
                  newIndirectPatternDetectorEntry.state := IndirectPatternDetectorState.CALCULATE_BASE_ADDRESSES
                  newIndirectPatternDetectorEntry.prefetchTableIndex := prefetchTableIndex
                  newIndirectPatternDetectorEntry.index1 := loadedValue
                  newIndirectPatternDetectorEntry.index2 := U(0, config.isa.xlen bits)
                  newIndirectPatternDetectorEntry.baseAddresses := newIndirectPatternDetectorEntry.baseAddresses.getZero
                  newIndirectPatternDetectorEntry.missCount := U(
                    0,
                    log2Up(baseAddressArrayLength + 1) bits
                  )
                  newIndirectPatternDetectorEntry.hasComparedCacheMiss := False
                  indirectPatternDetectorTable(
                    indirectPatternDetectorAllocationPointer
                  ) := newIndirectPatternDetectorEntry
                  indirectPatternDetectorAllocationPointer := indirectPatternDetectorAllocationPointer + 1
                }
              } otherwise {
                // verify the indirect access
                when(prefetchTableEntry.isVerifyingIndirectAccess) {
                  // the expected indirect access did not arrive
                  // (out of order scheduling can delay it by several index accesses)
                  when(
                    prefetchTableEntry.missedIndirectAccessCount =/= indirectVerificationLeniency
                  ) {
                    prefetchTableEntry.missedIndirectAccessCount := prefetchTableEntry.missedIndirectAccessCount + 1
                  } elsewhen (prefetchTableEntry.indirectHitCount =/= 0) {
                    prefetchTableEntry.indirectHitCount := prefetchTableEntry.indirectHitCount - 1
                    prefetchTableEntry.missedIndirectAccessCount := U(
                      0,
                      log2Up(indirectVerificationLeniency + 1) bits
                    )
                  } otherwise {
                    prefetchTableEntry.active := False
                  }
                }
                prefetchTableEntry.isVerifyingIndirectAccess := True
              }
            }
          } elsewhen (!ignoreLoad && !isLearningIndirectPattern) {
            when(prefetchTableEntry.active) {
              // the stride pattern restarted at another position:
              // keep the indirect pattern and only update the current address
              prefetchTableEntry.currentStrideAddress := responseAddress
              prefetchTableEntry.currentValue := loadedValue
              prefetchTableEntry.isVerifyingIndirectAccess := True
            } elsewhen (prefetchTableEntry.strideHitCount === 0) {
              // elsewhen / otherwise: the load at the pc does not match the pattern
              prefetchTableEntry := PrefetchTableEntry().getZero
            } otherwise {
              prefetchTableEntry.strideHitCount := prefetchTableEntry.strideHitCount - 1
            }
          } // otherwise: ignore it, don't increase (or decrease) hit count
        } otherwise {
          // the pc of the load is not registered in the prefetch table:
          // add new entry to the prefetch table, if there is a free slot
          val (tableHasInvalidEntry, invalidEntryIndex) =
            prefetchTable.sFindFirst(entry => !entry.valid)
          val tableHasFreeEntry = Bool()
          tableHasFreeEntry := tableHasInvalidEntry
          val newEntryIndex = UInt(log2Up(prefetchTableSize) bits)
          newEntryIndex := invalidEntryIndex
          when(!tableHasInvalidEntry) {
            // find entry with lowest hit count
            var currentLowestHitCount: UInt = U(0, log2Up(strideActivationHitCount + 1) bits)
            var entryFound: Bool = False
            for (i <- 0 until prefetchTableSize) {
              val isLearningIndirectPattern = indirectPatternDetectorTable.sExist(entry =>
                entry.state =/= IndirectPatternDetectorState.INVALID && entry.prefetchTableIndex === i
              )
              val isNewLowestHitCount = (!entryFound || prefetchTable(
                i
              ).strideHitCount < currentLowestHitCount) && !prefetchTable(
                i
              ).active && !isLearningIndirectPattern
              when(isNewLowestHitCount) {
                newEntryIndex := i
                tableHasFreeEntry := True
              }
              currentLowestHitCount =
                Mux(isNewLowestHitCount, prefetchTable(i).strideHitCount, currentLowestHitCount)
              entryFound = entryFound || isNewLowestHitCount
            }
          }
          // insert it in the table
          when(tableHasFreeEntry) {
            prefetchTable(newEntryIndex).valid := True
            prefetchTable(newEntryIndex).pc := responsePc(pcCompareBits - 1 downto 0)
            prefetchTable(newEntryIndex).currentStrideAddress := responseAddress
            prefetchTable(newEntryIndex).strideHitCount := U(
              0,
              log2Up(strideActivationHitCount + 1) bits
            )
            prefetchTable(newEntryIndex).active := False
            prefetchTable(newEntryIndex).currentValue := U(0, config.isa.xlen bits)
            prefetchTable(newEntryIndex).isVerifyingIndirectAccess := False
            prefetchTable(newEntryIndex).missedIndirectAccessCount := U(
              0,
              log2Up(indirectVerificationLeniency + 1) bits
            )
            prefetchTable(newEntryIndex).indirectBaseAddress := U(0, config.isa.xlen bits)
            prefetchTable(newEntryIndex).shiftIndex := U(0, log2Up(shiftValues.length) bits)
            prefetchTable(newEntryIndex).indirectHitCount := U(0, log2Up(maxHitCount + 1) bits)
            prefetchTable(newEntryIndex).prefetchDistance := U(
              0,
              log2Up(maxPrefetchDistance + 1) bits
            )
          }
        }

        // handle entries of the indirect pattern detector table that are waiting for cache misses
        when(isResponseCacheMiss) {
          for (i <- 0 until indirectPatternDetectorTableSize) {
            val indirectPatternDetectorEntry = indirectPatternDetectorTable(i)
            val isStrideAccess =
              prefetchTableEntryFound && indirectPatternDetectorEntry.prefetchTableIndex === prefetchTableIndex
            when(
              indirectPatternDetectorEntry.state =/= IndirectPatternDetectorState.INVALID && !isStrideAccess
                && !(isAllocatingIndirectPatternDetectorEntry && indirectPatternDetectorAllocationPointer === i)
            ) {
              val matchFound = Bool()
              matchFound := False
              val matchBaseAddress = UInt(config.isa.xlen bits)
              matchBaseAddress := 0
              val matchShiftIndex = UInt(log2Up(shiftValues.length) bits)
              matchShiftIndex := 0
              // look for a match
              when(
                indirectPatternDetectorEntry.state === IndirectPatternDetectorState.FIND_MATCH
                  && indirectPatternDetectorEntry.missCount =/= 0
              ) {
                indirectPatternDetectorEntry.hasComparedCacheMiss := True
                for ((shift, shiftIndex) <- shiftValues.zipWithIndex) {
                  val index1Shifted = shiftValue(indirectPatternDetectorEntry.index1, shift)
                  val index2Shifted = shiftValue(indirectPatternDetectorEntry.index2, shift)
                  val isIdenticalShiftedValue = index1Shifted === index2Shifted
                  val baseAddress = responseAddress - index2Shifted
                  for (j <- 0 until baseAddressArrayLength) {
                    when(
                      !isIdenticalShiftedValue && j < indirectPatternDetectorEntry.missCount &&
                        indirectPatternDetectorEntry.baseAddresses(shiftIndex)(j) === baseAddress
                    ) {
                      matchFound := True
                      matchBaseAddress := baseAddress
                      matchShiftIndex := shiftIndex
                    }
                  }
                }
              }

              when(matchFound) {
                // clear indirect pattern detector entry, activate indirect part of prefetch table entry
                val prefetchTableEntry =
                  prefetchTable(indirectPatternDetectorEntry.prefetchTableIndex)
                prefetchTableEntry.active := True
                prefetchTableEntry.indirectBaseAddress := matchBaseAddress
                prefetchTableEntry.shiftIndex := matchShiftIndex
                prefetchTableEntry.indirectHitCount := U(0, log2Up(maxHitCount + 1) bits)
                prefetchTableEntry.currentValue := U(0, config.isa.xlen bits)
                prefetchTableEntry.isVerifyingIndirectAccess := False
                prefetchTableEntry.missedIndirectAccessCount := U(
                  0,
                  log2Up(indirectVerificationLeniency + 1) bits
                )
                prefetchTableEntry.prefetchDistance := U(1, log2Up(maxPrefetchDistance + 1) bits)
                indirectPatternDetectorEntry := indirectPatternDetectorEntry.getZero
              } elsewhen (
                indirectPatternDetectorEntry.missCount =/= baseAddressArrayLength
              ) {
                // stage 1: calculate potential base addresses using stored index1,
                // if already in stage 2, but no match found and space in the array: also store it under index1
                for ((shift, shiftIndex) <- shiftValues.zipWithIndex) {
                  indirectPatternDetectorEntry.baseAddresses(shiftIndex)(
                    indirectPatternDetectorEntry.missCount.resize(
                      log2Up(baseAddressArrayLength) bits
                    )
                  ) := responseAddress - shiftValue(indirectPatternDetectorEntry.index1, shift)
                }
                indirectPatternDetectorEntry.missCount := indirectPatternDetectorEntry.missCount + 1
              }
            }
          }
        }

        // verify indirect pattern and increase hit count
        for (i <- 0 until prefetchTableSize) {
          val prefetchTableEntry = prefetchTable(i)
          val expectedIndirectAddress = calculateIndirectAddress(
            prefetchTableEntry.indirectBaseAddress,
            prefetchTableEntry.shiftIndex,
            prefetchTableEntry.currentValue
          )
          when(
            prefetchTableEntry.valid && prefetchTableEntry.active && prefetchTableEntry.isVerifyingIndirectAccess
          ) {
            when(expectedIndirectAddress === responseAddress) {
              prefetchTableEntry.isVerifyingIndirectAccess := False
              prefetchTableEntry.missedIndirectAccessCount := U(
                0,
                log2Up(indirectVerificationLeniency + 1) bits
              )
              when(prefetchTableEntry.indirectHitCount =/= maxHitCount) {
                prefetchTableEntry.indirectHitCount := prefetchTableEntry.indirectHitCount + 1
              }
              when(prefetchTableEntry.prefetchDistance =/= maxPrefetchDistance) {
                prefetchTableEntry.prefetchDistance := prefetchTableEntry.prefetchDistance + 1
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

    // when the DMP is active, it triggers upon loading an element of the stride array:
    // 1. it prefetches an element ahead in the array,
    // 2. it reads the value, adds it to the base of the indirect array, and
    // 3. prefetches the result
    val (activePrefetchTableEntryFound, activePrefetchTableIndex) =
      prefetchTable.sFindFirst(entry =>
        entry.pc === pc(
          pcCompareBits - 1 downto 0
        ) && entry.valid && entry.active && entry.indirectHitCount >= indirectActivationHitCount
      )
    when(activePrefetchTableEntryFound) {
      val activePrefetchTableEntry = prefetchTable(activePrefetchTableIndex)
      val prefetchTarget =
        address + (activePrefetchTableEntry.prefetchDistance << log2Up(config.isa.xlen / 8))
      val newPrefetchEntry = PrefetchQueueEntry()
      val addEntryToQueue = Bool()
      addEntryToQueue := True

      newPrefetchEntry.isValid := True
      newPrefetchEntry.targetAddress := prefetchTarget
      newPrefetchEntry.shiftIndex := activePrefetchTableEntry.shiftIndex
      newPrefetchEntry.indirectBaseAddress := activePrefetchTableEntry.indirectBaseAddress
      when(getCacheLineBits(storedCacheLineAddress) === getCacheLineBits(prefetchTarget)) {
        val word = getWordFromCacheLine(prefetchTarget, storedCacheLineValue)
        // dmp analyzes the word: leak
        noninterferenceSignal3 := word.asBits
        val indirectAddress = calculateIndirectAddress(
          activePrefetchTableEntry.indirectBaseAddress,
          activePrefetchTableEntry.shiftIndex,
          word
        )
        newPrefetchEntry.isTargetAddressPrefetched := True
        newPrefetchEntry.indirectAddress := indirectAddress
        newPrefetchEntry.isIndirectAddressValid := True
        when(!pointerHeuristic(prefetchTarget, indirectAddress)) {
          addEntryToQueue := False
        }
      } otherwise {
        newPrefetchEntry.isTargetAddressPrefetched := False
        newPrefetchEntry.indirectAddress := 0
        newPrefetchEntry.isIndirectAddressValid := False
      }
      when(addEntryToQueue) {
        addToPrefetchQueue(newPrefetchEntry)
      }
    }
  }

  override def notifyLoadResponse(address: UInt, pc: UInt, data: UInt, cacheHit: Boolean): Unit = {
    isReceivingLoadResponse := True
    if (cacheHit) {
      isResponseCacheMiss := False
    } else {
      isResponseCacheMiss := True
    }
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
        entry.isValid && entry.isTargetAddressPrefetched && !entry.isIndirectAddressValid && getCacheLineBits(
          entry.targetAddress
        ) === getCacheLineBits(address)
      ) {
        storedCacheLineAddress := address
        storedCacheLineValue := data
        val word = getWordFromCacheLine(entry.targetAddress, data)
        // dmp analyzes the word => leak
        noninterferenceSignal5(i * config.isa.xlen, config.isa.xlen bits) := word.asBits
        val indirectAddress =
          calculateIndirectAddress(entry.indirectBaseAddress, entry.shiftIndex, word)
        when(pointerHeuristic(entry.targetAddress, indirectAddress)) {
          prefetchQueueNext(i).indirectAddress := indirectAddress
          prefetchQueueNext(i).isIndirectAddressValid := True
        } otherwise {
          prefetchQueueNext(i).isValid := False
        }
      }
    }
  }

  override def getNextPrefetchTarget(id: UInt): UInt = {
    // check from old to new, for each entry check first the target, then the indirect address to prefetch
    val target = UInt(config.isa.xlen bits)
    target := 0
    var alreadyFound: Bool = False
    for (i <- prefetchQueueSize - 1 downto 0) {
      val entry = prefetchQueue(i)
      val validTargetAddress = entry.isValid && !entry.isTargetAddressPrefetched
      val validIndirectAddress = entry.isValid && entry.isIndirectAddressValid

      when(!alreadyFound) {
        when(validTargetAddress) {
          target := entry.targetAddress
          prefetchQueueNext(i).isTargetAddressPrefetched := True
        } elsewhen (validIndirectAddress) {
          target := entry.indirectAddress
          prefetchQueueNext(i).isValid := False
        }
      }

      alreadyFound = validTargetAddress || validIndirectAddress || alreadyFound
    }
    target
  }

  override def hasPrefetchTarget: Bool = {
    !busy && prefetchQueue.sExist(entry =>
      entry.isValid && (!entry.isTargetAddressPrefetched || entry.isIndirectAddressValid)
    )
  }
}
