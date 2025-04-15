package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.{Counter, Flow}

case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]])(implicit config: Config)
    extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle
  val rdbUpdated = Bool()
  val cdbUpdated = Bool()
  val willCdbUpdate = Bool()

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

  private val fenceDetectedNext = Bool()
  private val fenceDetected = RegNext(fenceDetectedNext).init(False)
  val isAvailable = (!isFull || willRetire) && !fenceDetectedNext

  val lastSpeculativeCFInstruction = Reg(Flow(UInt(indexBits)))

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry(retirementRegisters)
  pushedEntry := RobEntry(retirementRegisters).getZero

  val previousStoreBuffer = RegInit(UInt(config.xlen bits).getZero)
  val previousStoreAddress =
    if (config.addressBasedPsf) RegInit(UInt(config.xlen bits).getZero) else null

  val ssbMispredictions = RegInit(UInt(config.xlen bits).getZero)
  val ssbPredictions = RegInit(UInt(config.xlen bits).getZero)
  val psfMispredictions = RegInit(UInt(config.xlen bits).getZero)
  val psfPredictions = RegInit(UInt(config.xlen bits).getZero)

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    isFull := False
    lastSpeculativeCFInstruction.setIdle()
    fenceDetected := False
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
    ret
  }

  private def relativeIndexForAbsolute(absolute: UInt): UInt = {
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

  def pushEntry(): (UInt, EntryMetadata) = {
    val issueStage = pipeline.issuePipeline.stages.last

    pushInCycle := True
    pushedEntry.rdbUpdated := False
    pushedEntry.cdbUpdated := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := issueStage
      .output(pipeline.data.PC)
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
      spec.isSpeculativeMD(pushedEntry.registerMap) := False
    }

    val rs1 = Flow(UInt(5 bits))
    val rs2 = Flow(UInt(5 bits))

    rs1.valid := issueStage.output(pipeline.data.RS1_TYPE) === RegisterType.GPR
    rs1.payload := issueStage.output(pipeline.data.RS1)

    rs2.valid := issueStage.output(pipeline.data.RS2_TYPE) === RegisterType.GPR
    rs2.payload := issueStage.output(pipeline.data.RS2)

    (newestIndex.value, bookkeeping(rs1, rs2))
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

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    // prevent CDB updates from PSF-predicted loads for now
    when(!robEntries(cdbMessage.robIndex).cdbUpdated) {
      robEntries(cdbMessage.robIndex).cdbUpdated := True
      robEntries(cdbMessage.robIndex).registerMap
        .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue

      val lsu = pipeline.service[LsuService]
      lsu.psfAddress(robEntries(cdbMessage.robIndex).registerMap) := lsu.psfAddress(
        cdbMessage.metadata
      )

      pipeline.serviceOption[SpeculationService] foreach { spec =>
        // mark PSF speculation
        spec.isSpeculativeMD(robEntries(cdbMessage.robIndex).registerMap) := spec.isSpeculativeMD(
          cdbMessage.metadata
        )

        // if this instruction's CF change was correctly predicted, disregard it as the most recent speculative instruction
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

  def hasPendingStoreForEntry(robIndex: UInt, address: UInt): (Bool, Bool) = {
    val foundMatch = Bool()
    foundMatch := False

    val foundUnknown = Bool()
    foundUnknown := False

    val wordAddress = byte2WordAddress(address)

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
      val isOlder = relativeIndexForAbsolute(index) < relativeIndexForAbsolute(robIndex)

      when(
        isValidAbsoluteIndex(nth)
          && isOlder
          && entryIsStore
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

  private def hasSpeculatingLoad: Bool = {
    val found = Bool()
    found := False

    val lsuService = pipeline.service[LsuService]
    val oldestEntry = robEntries(oldestIndex.value)
    val storeValue =
      oldestEntry.registerMap.element(pipeline.data.RS2_DATA.asInstanceOf[PipelineData[Data]])

    val wordAddress = byte2WordAddress(lsuService.addressOfBundle(oldestEntry.registerMap))

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val entryIsLoad = lsuService.operationOfBundle(entry.registerMap) === LsuOperationType.LOAD
      val entryAddressValid = lsuService.addressValidOfBundle(entry.registerMap)
      val entryAddress = lsuService.addressOfBundle(entry.registerMap)
      val entryWordAddress = byte2WordAddress(entryAddress)
      val addressesMatch = entryWordAddress === wordAddress
      val loadValue =
        entry.registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])
      val valueValid = entry.cdbUpdated

      val speculative = pipeline.service[LsuService].stlSpeculation(entry.registerMap)

      val entriesMatch: Bool = if (config.addressBasedSsb) {
        isValidAbsoluteIndex(
          nth
        ) && entryIsLoad && (entryAddressValid && addressesMatch && speculative)
      } else {
        isValidAbsoluteIndex(
          nth
        ) && entryIsLoad && (entryAddressValid && addressesMatch && speculative) && (!valueValid || storeValue =/= loadValue)
      }

      when(entriesMatch) {
        found := True
      }
    }
    found
  }

  def onRdbMessage(rdbMessage: RdbMessage): Unit = {
    robEntries(rdbMessage.robIndex).registerMap := rdbMessage.registerMap
    robEntries(rdbMessage.robIndex).willCdbUpdate := rdbMessage.willCdbUpdate

    when(pipeline.service[CsrService].isCsrInstruction(rdbMessage.registerMap)) {
      pipeline
        .service[JumpService]
        .jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
    }

    val lsu = pipeline.service[LsuService]

    if (config.stlSpec) {
      when(
        lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.STORE
      ) {
        previousStoreBuffer := rdbMessage.registerMap.elementAs[UInt](
          pipeline.data.RS2_DATA.asInstanceOf[PipelineData[Data]]
        )
        if (config.addressBasedPsf) {
          previousStoreAddress := lsu.addressOfBundle(rdbMessage.registerMap)
        }
      }

      // detect wrongly forwarded PSF
      when(
        lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.LOAD && robEntries(
          rdbMessage.robIndex
        ).cdbUpdated
      ) {
        val psfMismatch: Bool = if (config.addressBasedPsf) {
          lsu.psfAddress(robEntries(rdbMessage.robIndex).registerMap) =/= lsu.addressOfBundle(
            rdbMessage.registerMap
          )
        } else {
          rdbMessage.registerMap.element(
            pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]
          ) =/= robEntries(rdbMessage.robIndex).registerMap
            .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])
        }

        when(psfMismatch) {
          psfMispredictions := psfMispredictions + 1
          pipeline
            .service[JumpService]
            .jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
        } otherwise {
          psfPredictions := psfPredictions + 1
        }
      }
    }

    robEntries(rdbMessage.robIndex).rdbUpdated := True
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
        // removing the oldest entry
        updatedOldestIndex := oldestIndex.valueNext
        oldestIndex.increment()
        willRetire := True
        isFullNext := False

        if (config.stlSpec) {
          when(
            lsuService.operationOfBundle(
              oldestEntry.registerMap
            ) === LsuOperationType.STORE
          ) {
            when(hasSpeculatingLoad) {
              ssbMispredictions := ssbMispredictions + 1
              pipeline
                .service[JumpService]
                .flushPipeline(ret)
            }
          }
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
  }
}
