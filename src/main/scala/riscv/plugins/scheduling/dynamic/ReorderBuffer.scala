package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]])(implicit config: Config)
    extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle
  val rdbUpdated = Bool()
  val cdbUpdated = Bool()
  val willCdbUpdate = Bool()
  val invalidated = Bool()
  val preventSsb = Bool()

  override def clone(): RobEntry = {
    RobEntry(retirementRegisters)
  }
}

case class RsData(indexBits: BitCount)(implicit config: Config) extends Bundle {
  val updatingInstructionFound = Bool()
  val updatingInstructionFinished = Bool()
  val updatingInstructionIndex = UInt(indexBits)
  val updatingInstructionValue = UInt(config.xlen bits)
  val updatingInstructionLoadSpeculation = Bool()
}

case class EntryMetadata(indexBits: BitCount)(implicit config: Config) extends Bundle {
  val rs1Data = Flow(RsData(indexBits))
  val rs2Data = Flow(RsData(indexBits))
  val preventPsf = Bool()

  override def clone(): EntryMetadata = {
    EntryMetadata(indexBits)
  }
}

/** Terminology:
  *   - absolute index: the actual index of an entry in the circular buffer
  *   - relative index: an index that shows the order of instructions inserted into the ROB, 0 being
  *     the oldest
  *
  * Relative indices are internal to the ROB, outside components only see the absolute index of an
  * entry
  */
class ReorderBuffer(
    pipeline: DynamicPipeline,
    robCapacity: Int,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area
    with CdbListener
    with Resettable {
  def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry(retirementRegisters).getZero))
  val oldestIndex = Counter(capacity)
  val newestIndex = Counter(capacity)
  private val isFullNext = Bool()
  private val isFull = RegNext(isFullNext).init(False)
  private val willRetire = False

  private val flushCounter = Reg(UInt(config.xlen bits)).init(0)
  private val softFlushCounter = Reg(UInt(config.xlen bits)).init(0)

  private val fenceDetectedNext = Bool()
  private val fenceDetected = RegNext(fenceDetectedNext).init(False)

  val isAvailable = (!isFull || willRetire) && !fenceDetectedNext

  val lastSpeculativeCFInstruction = Reg(Flow(UInt(indexBits)))

  val softResetThisCycle = False
  private val hardResetThisCycle = False
  val newestAtSoftReset = Reg(UInt(indexBits))
  val softResetTrigger = Reg(Flow(UInt(indexBits)))
  val currentSoftResetTrigger = Flow(UInt(indexBits))
  currentSoftResetTrigger.setIdle()
  val currentCdbUpdate = Flow(UInt(indexBits))
  currentCdbUpdate.setIdle()
  val currentRdbUpdate = Flow(UInt(indexBits))
  currentRdbUpdate.setIdle()

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry(retirementRegisters)
  pushedEntry := RobEntry(retirementRegisters).getZero

  /*
   * data structures related to speculative store bypass (SSB)
   */

  val ssbMispredictions = RegInit(UInt(config.xlen bits).getZero)
  val ssbPredictions = RegInit(UInt(config.xlen bits).getZero)

  private val currentlyInsertingStore = Flow(UInt(config.xlen bits))
  currentlyInsertingStore.setIdle()

  val ssbPredictorNumEntries = 12
  private val ssbPredictorEntries =
    Vec.fill(ssbPredictorNumEntries)(RegInit(UInt(config.xlen bits).getZero))
  private val ssbPredictorCounter = Counter(ssbPredictorNumEntries)

  def findSsbPredictorEntry(pc: UInt): Bool = {
    val result = False
    for (i <- 0 until ssbPredictorNumEntries) {
      when(ssbPredictorEntries(i) === pc) {
        result := True
      }
    }
    result
  }

  def addSsbPredictorEntry(pc: UInt): Unit = {
    val index = ssbPredictorCounter.value
    ssbPredictorEntries(index) := pc
    ssbPredictorCounter.increment()
  }

  /*
   * data structures related to predictive store forwarding (PSF)
   */

  val psfMispredictions = RegInit(UInt(config.xlen bits).getZero)
  val psfPredictions = RegInit(UInt(config.xlen bits).getZero)
  val totalLoads = RegInit(UInt(config.xlen bits).getZero)

  val previousStoreBuffer = RegInit(UInt(config.xlen bits).getZero)
  val previousStoreAddress =
    if (config.addressBasedPsf) RegInit(UInt(config.xlen bits).getZero) else null

  val psfPredictorNumEntries = 12
  private val psfPredictorEntries =
    Vec.fill(psfPredictorNumEntries)(RegInit(UInt(config.xlen bits).getZero))
  private val psfPredictorCounter = Counter(psfPredictorNumEntries)

  def findPsfPredictorEntry(pc: UInt): Bool = {
    val result = False
    for (i <- 0 until psfPredictorNumEntries) {
      when(psfPredictorEntries(i) === pc) {
        result := True
      }
    }
    result
  }

  def addPsfPredictorEntry(pc: UInt): Unit = {
    val index = psfPredictorCounter.value
    psfPredictorEntries(index) := pc
    psfPredictorCounter.increment()
  }

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    isFull := False
    lastSpeculativeCFInstruction.setIdle()
    fenceDetected := False
    softResetTrigger.setIdle()
    for (nth <- 0 until capacity) {
      robEntries(nth).invalidated := False
    }
  }

  /** A builder pattern helper used to attach specific callbacks or actions to a flush request
   * depending on the type of flush (soft or hard) granted by the arbiter.
   *
   * @param req The parent [[FlushPort]] associated with this builder.
   */
  class FlushActionBuilder(req: FlushPort) {
    /** Registers a block of code to execute if a soft flush is granted. */
    def onSoftFlush(block: => Unit): FlushActionBuilder = {
      when(req.grantedSoftFlush) { block }
      this
    }

    /** Registers a block of code to execute if a hard flush is granted. */
    def onHardFlush(block: => Unit): FlushActionBuilder = {
      when(req.grantedHardFlush) { block }
      this
    }

    /** Registers a block of code to execute if either a soft or hard flush is granted. */
    def onAnyFlush(block: => Unit): FlushActionBuilder = {
      when(req.grantedAnyFlush) { block }
      this
    }
  }

  /** Represents a port through which pipeline services can request a pipeline flush.
   * Automatically registers itself into the ROB's internal `_flushRequests` list upon creation.
   */
  class FlushPort(implicit config: Config) extends Bundle {
    // Automatically register this port into the global arbitration pool so the correct logic can be added later.
    _flushRequests += this

    /** Indicates whether this flush request is active. */
    val valid            = Bool()
    /** The ROB index of the last correct instruction (typically the instruction requesting the flush). */
    val lastCorrectId    = UInt(indexBits)
    /** The target program counter (PC) where execution should resume after the flush. */
    val nextPc           = UInt(config.xlen bits)

    /** Set if this request wins and is serviced via a soft flush. */
    val grantedSoftFlush = Bool()
    /** Set if this request is serviced via a hard flush. */
    val grantedHardFlush = Bool()
    /** Helper indicating if any flush type was granted to this request. */
    def grantedAnyFlush  = grantedSoftFlush || grantedHardFlush

    /** Initializes the flush port with target metadata and defaults flags to false.
     *
     * @param lastCorrectId The ROB index of the instruction requesting the flush.
     * @param nextPc The target PC to jump to after flushing.
     * @return This initialized [[FlushPort]] instance.
     */
    def init(lastCorrectId: UInt, nextPc: UInt): FlushPort = {
      this.lastCorrectId := lastCorrectId
      this.nextPc := nextPc
      this.valid := False
      this.grantedSoftFlush := False
      this.grantedHardFlush := False
      this
    }

    /** Activates the flush request and returns an action builder to register callbacks.
     *
     * @return A [[FlushActionBuilder]] for handling grant callbacks.
     */
    def request(): FlushActionBuilder = {
      this.valid := True
      new FlushActionBuilder(this)
    }

    /** Convenience method to trigger a flush due to a Predictive Store Forwarding (PSF) misprediction.
     * Automatically trains the PSF predictor and increments the misprediction counter when granted.
     * TODO: Should be moved to a PSF specific module later.
     * @param pc The PC of the mispredicted instruction.
     */
    def requestPsf(pc: UInt): Unit = {
      request().onAnyFlush {
        addPsfPredictorEntry(pc)
        psfMispredictions := psfMispredictions + 1
      }
    }

    /** Convenience method to trigger a flush due to a Speculative Store Bypass (SSB) misprediction.
     * Automatically trains the SSB predictor and increments the misprediction counter when granted.
     * TODO: Should be moved to a SSB specific module later.
     *
     * @param pc The PC of the mispredicted instruction.
     */
    def requestSsb(pc: UInt): Unit = {
      request().onAnyFlush {
        addSsbPredictorEntry(pc)
        ssbMispredictions := ssbMispredictions + 1
      }
    }
  }

  /** Collection of all active flush ports instantiated during code generation. */
  private val _flushRequests = ArrayBuffer[FlushPort]()

  /** Evaluates all pending flush requests, arbitrates to find the oldest requesting instruction,
   * and executes the appropriate flush mechanism (Soft Flush vs. Hard Flush).
   *
   * A **Soft Flush** is preferred as it immediately invalidates only younger, speculative instructions
   * in the ROB and redirects the fetch stage without waiting for the faulting instruction to retire.
   *
   * Due to hardware constraints, there can only be one soft flush at a time. If it is already taken,
   * a **Hard Flush** acts as a fallback by marking the instruction bundle to jump upon retirement
   * completely wiping the pipeline when it reaches the end of the ROB.
   */
  def processFlushes(): Unit = {
    var winnerFound: Bool = False
    var winningIndex: UInt = U(0, indexBits)
    var winningPc: UInt = U(0, config.xlen bits)

    // Iterate through all registered flush ports to find the oldest request in this cycle.
    // In an out-of-order execution engine, the oldest instruction always takes priority to ensure
    // program order correctness.
    for (req <- _flushRequests) {
      val older = isOlder(req.lastCorrectId, winningIndex)
      val takeThis = req.valid && (!winnerFound || older)

      winnerFound = winnerFound || req.valid
      winningIndex = Mux(takeThis, req.lastCorrectId, winningIndex)
      winningPc = Mux(takeThis, req.nextPc, winningPc)
    }

    when(winnerFound) {
      val winningEntry = robEntries(winningIndex)

      // Ensure the winning entry hasn't already been invalidated by an earlier flush
      // and that a global hard reset isn't currently taking place.
      when(!winningEntry.invalidated && !hardResetThisCycle) {

        // Handle edge cases where multiple ports request a flush from the EXACT same instruction index.
        // We create a bitmask of all valid requests matching the winning index and pick the first one.
        val winnerMask = B(_flushRequests.map(req => req.valid && req.lastCorrectId === winningIndex))
        val singleWinnerOH = OHMasking.firstV2(winnerMask)

        // Check if the current soft reset is still valid or if it can be replaced.
        val currentTriggerRetiring = softResetTrigger.valid &&
          softResetTrigger.payload === oldestIndex.value &&
          willRetire
        val currentTriggerValid = softResetTrigger.valid && !currentTriggerRetiring
        val isRedundant = currentTriggerValid && winningIndex === softResetTrigger.payload || pipeline.service[JumpService].jumpOfBundle(winningEntry.registerMap)

        // We can perform a soft flush if there is no active trigger, or if the new flush is older than the existing trigger.
        val canSoftFlush = !currentTriggerValid || isOlder(winningIndex, softResetTrigger.payload)

        when(isRedundant) {
          // Do nothing; the pipeline is already scheduled to flush from this instruction point.
        } elsewhen(canSoftFlush) {
          // --- SOFT FLUSH EXECUTION ---
          fenceDetected := False
          softResetThisCycle := True
          softResetTrigger.push(winningIndex)
          currentSoftResetTrigger.push(winningIndex)
          newestAtSoftReset := newestIndex.value

          pipeline.issuePipeline.service[JumpService].jump(winningPc)
          lastSpeculativeCFInstruction.setIdle()

          // Grant the soft flush signal to the winning port to trigger its `onSoftFlush` / `onAnyFlush` callbacks.
          for ((req, i) <- _flushRequests.zipWithIndex) {
            when(singleWinnerOH(i)) {
              req.grantedSoftFlush := True
            }
          }

          // Invalidate all ROB entries younger than the winning instruction.
          for (relative <- 0 until capacity) {
            val absolute = absoluteIndexForRelative(relative).resized
            val entry = robEntries(absolute)
            when(isValidAbsoluteIndex(absolute)) {
              when(relative > relativeIndexForAbsolute(winningIndex)) {
                entry.invalidated := True
              } otherwise {
                // For surviving older entries, update the latest speculative control-flow tracking.
                pipeline.serviceOption[SpeculationService] foreach { spec =>
                  when(spec.isSpeculativeCF(entry.registerMap)) {
                    lastSpeculativeCFInstruction.push(absolute)
                  }
                }
              }
            }
          }
          softFlushCounter := softFlushCounter + 1
        } otherwise {
          // --- HARD FLUSH FALLBACK ---
          // If a soft flush cannot be applied, defer the flush until the winning instruction retires.
          // This is done by asserting the jump flag on the instruction's register bundle.
          pipeline.service[JumpService].jumpOfBundle(winningEntry.registerMap) := True

          // Grant the hard flush signal to the winning port to trigger its `onHardFlush` / `onAnyFlush` callbacks.
          for ((req, i) <- _flushRequests.zipWithIndex) {
            when(singleWinnerOH(i)) {
              req.grantedHardFlush := True
            }
          }
        }
      }
    }
  }

  private def byte2WordAddress(address: UInt) = {
    address(config.xlen - 1 downto log2Up(config.xlen / 8))
  }

  private def isValidAbsoluteIndex(index: UInt): Bool = {
    val ret = Bool()

    val oldest = UInt(indexBits)
    val newest = UInt(indexBits)
    oldest := oldestIndex.value
    newest := newestIndex.value

    when(isFull) {
      ret := True
    } elsewhen (oldest === newest && !isFull) { // empty
      ret := False
    } elsewhen (newest > oldest) { // normal order
      ret := index >= oldest && index < newest
    } otherwise { // wrapping
      ret := index >= oldest || index < newest
    }

    when(robEntries(index.resized).invalidated) {
      ret := False
    }

    ret
  }

  def relativeIndexForAbsolute(absolute: UInt): UInt = {
    val adjustedIndex = UInt(32 bits)
    when(absolute >= oldestIndex.value) {
      adjustedIndex := (absolute.resized - oldestIndex.value).resized
    } otherwise {
      val remainder = capacity - oldestIndex.value
      adjustedIndex := (absolute + remainder).resized
    }
    adjustedIndex
  }

  private def absoluteIndexForRelative(relative: UInt): UInt = {
    val absolute = UInt(32 bits)
    val adjusted = UInt(32 bits)
    val oldestResized = UInt(32 bits)
    oldestResized := oldestIndex.value.resized
    absolute := oldestResized + relative
    when(absolute >= capacity) {
      adjusted := absolute - capacity
    } otherwise {
      adjusted := absolute
    }
    adjusted
  }

  def isOlder(a: UInt, b: UInt): Bool = {
    relativeIndexForAbsolute(a) < relativeIndexForAbsolute(b)
  }

  def isYounger(a: UInt, b: UInt): Bool = {
    relativeIndexForAbsolute(a) > relativeIndexForAbsolute(b)
  }

  def pushEntry(): (UInt, EntryMetadata) = {
    val issueStage = pipeline.issuePipeline.stages.last

    pushInCycle := True
    pushedEntry.rdbUpdated := False
    pushedEntry.cdbUpdated := False
    pushedEntry.invalidated := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := issueStage
      .output(pipeline.data.PC)
    pushedEntry.registerMap.element(
      pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
    ) := issueStage
      .output(pipeline.data.NEXT_PC)
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := issueStage
      .output(pipeline.data.RD)
    pushedEntry.registerMap.element(
      pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
    ) := issueStage.output(pipeline.data.RD_TYPE)
    pipeline.service[LsuService].operationOfBundle(pushedEntry.registerMap) := pipeline
      .service[LsuService]
      .operationOutput(issueStage)
    pipeline.service[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False

    when(pipeline.service[FenceService].isFence(issueStage)) {
      fenceDetected := True
    }

    pipeline.service[LsuService].stlSpeculation(pushedEntry.registerMap) := False

    pipeline.serviceOption[SpeculationService] foreach { spec =>
      when(spec.isSpeculativeCFOutput(issueStage)) {
        lastSpeculativeCFInstruction.push(newestIndex)
      }
      spec.isSpeculativeCF(pushedEntry.registerMap) := spec.isSpeculativeCFOutput(issueStage)
      spec.isSpeculativeMD(pushedEntry.registerMap) := False
    }

    val rs1 = Flow(UInt(5 bits))
    val rs2 = Flow(UInt(5 bits))

    rs1.valid := issueStage.output(pipeline.data.RS1_TYPE) === RegisterType.GPR
    rs1.payload := issueStage.output(pipeline.data.RS1)

    rs2.valid := issueStage.output(pipeline.data.RS2_TYPE) === RegisterType.GPR
    rs2.payload := issueStage.output(pipeline.data.RS2)

    val meta = bookkeeping(rs1, rs2)

    if (config.stlSpec) {
      meta.preventPsf := findPsfPredictorEntry(issueStage.output(pipeline.data.PC))
      pushedEntry.preventSsb := findSsbPredictorEntry(issueStage.output(pipeline.data.PC))
    } else {
      meta.preventPsf := False
      pushedEntry.preventSsb := False
    }

    (newestIndex.value, meta)
  }

  private def bookkeeping(rs1Id: Flow[UInt], rs2Id: Flow[UInt]): EntryMetadata = {
    val meta = EntryMetadata(indexBits)
    meta.rs1Data.payload.assignDontCare()
    meta.rs2Data.payload.assignDontCare()

    meta.rs1Data.valid := rs1Id.valid
    meta.rs2Data.valid := rs2Id.valid

    def rsUpdate(rsId: Flow[UInt], index: UInt, entry: RobEntry, rsMeta: RsData): Unit = {
      when(
        rsId.valid
          && rsId.payload =/= 0
          && entry.registerMap.element(
            pipeline.data.RD.asInstanceOf[PipelineData[Data]]
          ) === rsId.payload
          && entry.registerMap.element(
            pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
          ) === RegisterType.GPR
      ) {
        rsMeta.updatingInstructionFound := True
        rsMeta.updatingInstructionFinished := (entry.cdbUpdated || entry.rdbUpdated)
        rsMeta.updatingInstructionIndex := index
        rsMeta.updatingInstructionValue := entry.registerMap.elementAs[UInt](
          pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]
        )
        pipeline.serviceOption[SpeculationService] foreach { spec =>
          rsMeta.updatingInstructionLoadSpeculation := spec.isSpeculativeMD(entry.registerMap)
        }
      }
    }

    // loop through valid values and return the freshest if present
    for (relative <- 0 until capacity) {
      val absolute = absoluteIndexForRelative(relative).resized
      val entry = robEntries(absolute)

      when(isValidAbsoluteIndex(absolute)) {
        rsUpdate(rs1Id, absolute, entry, meta.rs1Data.payload)
        rsUpdate(rs2Id, absolute, entry, meta.rs2Data.payload)
      }
    }
    meta
  }

  override def onCdbMessage(cdbMessage: Flow[CdbMessage]): Unit = {
    // TODO: the PSF update logic is probably way too complicated...
    val lsu = pipeline.service[LsuService]

    val entry = robEntries(cdbMessage.robIndex)
    val pc = entry.registerMap
      .elementAs[UInt](pipeline.data.PC.asInstanceOf[PipelineData[Data]])
    val nextPc = entry.registerMap
      .elementAs[UInt](pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]])
    val flushPort = (new FlushPort).init(cdbMessage.robIndex, nextPc)

    def processValidEntry(): Unit = {
      entry.cdbUpdated := True
      entry.registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
    }

    when(cdbMessage.valid) {
      if (config.addressBasedPsf) {
        switch(lsu.psfState(cdbMessage.metadata)) {
          is(PsfState.NONE) {
            processValidEntry()
          }
          is(PsfState.PREDICTION) {
            psfPredictions := psfPredictions + 1
          }
          is(PsfState.WARNING) {
            // Request a flush
            flushPort.requestPsf(pc)
          }
          is(PsfState.MISS) {
            // Request a flush (this may be redundant but it is technically possible that the WARNING message has not
            // arrived yet.). The flush is still necessary because the reservation stations received the incorrect value
            // from the prediction.
            flushPort.requestPsf(pc)

            // This message does contain the correct values, so store the values
            processValidEntry()
          }
        }
      } else {
        processValidEntry()
      }

      when(!entry.cdbUpdated) {
        if (config.addressBasedPsf) {
          lsu.psfAddress(entry.registerMap) := lsu.psfAddress(cdbMessage.metadata)
        }

        pipeline.serviceOption[SpeculationService] foreach { spec =>
          spec.isSpeculativeMD(entry.registerMap) := spec.isSpeculativeMD(cdbMessage.metadata)
          spec.isSpeculativeCF(entry.registerMap) := spec.isSpeculativeCF(cdbMessage.metadata)

          when(
            lastSpeculativeCFInstruction.valid &&
              lastSpeculativeCFInstruction.payload === cdbMessage.robIndex &&
              !spec.isSpeculativeCF(cdbMessage.metadata)
          ) {
            lastSpeculativeCFInstruction := spec.speculationDependency(cdbMessage.metadata).resized
          }
        }
      }
    }
  }

  def hasPendingStoreForEntry(robIndex: UInt, address: UInt): (Bool, Bool) = {
    val foundMatch = Bool()
    foundMatch := False

    val foundUnknown = Bool()
    foundUnknown := False

    val wordAddress = byte2WordAddress(address)

    when(currentlyInsertingStore.valid) {
      when(byte2WordAddress(currentlyInsertingStore.payload) === wordAddress) {
        foundMatch := True
      }
    }

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val lsuService = pipeline.service[LsuService]
      val entryIsStore = lsuService.operationOfBundle(entry.registerMap) === LsuOperationType.STORE
      val entryAddressValid = lsuService.addressValidOfBundle(entry.registerMap)
      val entryAddress = lsuService.addressOfBundle(entry.registerMap)
      val entryWordAddress = byte2WordAddress(entryAddress)
      val addressesMatch = entryWordAddress === wordAddress

      when(
        isValidAbsoluteIndex(nth) && isOlder(index, robIndex)  && entryIsStore
      ) {
        when(entryAddressValid && addressesMatch) {
          foundMatch := True
        }
        when(!entryAddressValid) {
          foundUnknown := True
        }
      }
    }

    if (config.stlSpec) {
      when(foundUnknown && !foundMatch) {
        ssbPredictions := ssbPredictions + 1
      }
    }
    (foundMatch, foundUnknown)
  }

  private def hasSpeculatingLoad(storeIndex: UInt, storeValue: UInt, storeAddress: UInt): Bool = {
    val found = Bool()
    found := False

    val lsuService = pipeline.service[LsuService]
    val wordAddress = byte2WordAddress(storeAddress)

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val entryIsLoad = lsuService.operationOfBundle(entry.registerMap) === LsuOperationType.LOAD
      val entryAddressValid = lsuService.addressValidOfBundle(entry.registerMap)
      val entryAddress = lsuService.addressOfBundle(entry.registerMap)
      val entryWordAddress = byte2WordAddress(entryAddress)
      val addressesMatch = entryWordAddress === wordAddress
      val loadValue: UInt =
        entry.registerMap.elementAs[UInt](pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])
      val valueValid = entry.cdbUpdated
      val younger = isYounger(index, storeIndex)

      val speculative = pipeline.service[LsuService].stlSpeculation(entry.registerMap)

      val entriesMatch: Bool = if (config.addressBasedSsb) {
        isValidAbsoluteIndex(
          nth
        ) && entryIsLoad && younger && (entryAddressValid && addressesMatch && speculative)
      } else {
        isValidAbsoluteIndex(
          nth
        ) && entryIsLoad && younger && (entryAddressValid && addressesMatch && speculative) && (!valueValid || storeValue =/= loadValue)
      }

      when(entriesMatch) {
        found := True
      }
    }
    found
  }

  def onRdbMessage(rdbMessage: Flow[RdbMessage]): Unit = {
    val btb = pipeline.service[BranchTargetPredictorService]
    val jmp = pipeline.service[JumpService]
    val lsu = pipeline.service[LsuService]

    val entry = robEntries(rdbMessage.robIndex)
    val pc = entry.registerMap.elementAs[UInt](pipeline.data.PC.asInstanceOf[PipelineData[Data]])
    val nextPc = rdbMessage.registerMap.elementAs[UInt](pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]])

    val flushPort = (new FlushPort).init(rdbMessage.robIndex, nextPc)

    when(rdbMessage.valid) {

      // Copy all pipeline data
      entry.registerMap := rdbMessage.registerMap
      entry.willCdbUpdate := rdbMessage.willCdbUpdate

      // Exception 1: Branch Misprediction
      when(nextPc =/= btb.predictedPc(rdbMessage.registerMap)) {
        flushPort.request().onSoftFlush {
          btb.preventFlush(entry.registerMap)
        }
      }

      // Exception 2: CSR Serialization
      when(pipeline.service[CsrService].isCsrInstruction(rdbMessage.registerMap)) {
        flushPort.request().onSoftFlush {
          jmp.jumpOfBundle(entry.registerMap) := True
        }
      }

      if (config.stlSpec) {
        // Exception 3: SSB
        when(
          lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.STORE
        ) {
          val storeValue = rdbMessage.registerMap.elementAs[UInt](
            pipeline.data.RS2_DATA.asInstanceOf[PipelineData[Data]]
          )
          val storeAddress = lsu.addressOfBundle(rdbMessage.registerMap)
          currentlyInsertingStore.push(storeAddress)
          when(lsu.width(rdbMessage.registerMap) === LsuAccessWidth.W) {
            // for now, we only predict word memory operation
            previousStoreBuffer := storeValue
            if (config.addressBasedPsf) {
              previousStoreAddress := storeAddress
            }
          }

          when(hasSpeculatingLoad(rdbMessage.robIndex, storeValue, storeAddress)) {
            flushPort.requestSsb(pc)
          }
        }

        // Exception 4: Predictive Store Forwarding (PSF)
        if (config.addressBasedPsf) {
          when(lsu.psfState(rdbMessage.registerMap) === PsfState.MISS) {
            flushPort.requestPsf(pc)
          }
        } else {
          // Value-based PSF Fallback
          when(lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.LOAD && entry.cdbUpdated) {
            val psfMismatch = rdbMessage.registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) =/=
              entry.registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])

            when(psfMismatch) {
              flushPort.requestPsf(pc)
            }
          }
        }
      }

      entry.rdbUpdated := True
    }
  }

  def build(): Unit = {
    isFullNext := isFull
    fenceDetectedNext := fenceDetected
    val oldestEntry = robEntries(oldestIndex.value)
    val updatedOldestIndex = UInt(indexBits)
    updatedOldestIndex := oldestIndex.value
    val isEmpty = oldestIndex.value === newestIndex.value && !isFull

    val ret = pipeline.retirementStage
    ret.arbitration.isValid := False
    ret.arbitration.isStalled := False

    when(pipeline.service[FenceService].isFence(ret)) {
      fenceDetectedNext := False
    }

    for (register <- retirementRegisters.keys) {
      ret.input(register) := oldestEntry.registerMap.element(register)
    }

    val lsuService = pipeline.service[LsuService]

    // FIXME this doesn't seem the correct place to do this...
    ret.connectOutputDefaults()
    ret.connectLastValues()

    when(
      !isEmpty && oldestEntry.rdbUpdated && (oldestEntry.cdbUpdated || !oldestEntry.willCdbUpdate)
    ) {
      ret.arbitration.isValid := True

      when(ret.arbitration.isDone) {
        willRetire := True
        isFullNext := False

        when(
          softResetTrigger.valid && oldestIndex === softResetTrigger.payload && !hardResetThisCycle
        ) {
          when(!softResetThisCycle) {
            softResetTrigger.setIdle()
          }
          // skip over transient instructions
          oldestIndex := newestAtSoftReset
          updatedOldestIndex := newestAtSoftReset
        } otherwise {
          // removing the oldest entry
          updatedOldestIndex := oldestIndex.valueNext
          oldestIndex.increment()
        }
      }
    }

    when(pushInCycle) {
      robEntries(newestIndex.value) := pushedEntry
      val updatedNewest = newestIndex.valueNext
      newestIndex.increment()
      when(updatedOldestIndex === updatedNewest) {
        isFullNext := True
      }
    }
  }

  override def pipelineReset(): Unit = {
    reset()
    flushCounter := flushCounter + 1
    hardResetThisCycle := True
  }
}