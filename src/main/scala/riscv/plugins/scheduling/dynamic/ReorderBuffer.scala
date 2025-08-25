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
  val currentCdbSoftReset = Bool()
  currentCdbSoftReset := False
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

  /* data structures related to predictive store forwarding (PSF)
   *
   */

  val psfMispredictions = RegInit(UInt(config.xlen bits).getZero)
  val psfPredictions = RegInit(UInt(config.xlen bits).getZero)

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

  private def softFlush(lastCorrectId: UInt, nextPc: UInt): Bool = {
    val ret = False
    // prevent flushes caused by transient instructions
    when(!robEntries(lastCorrectId).invalidated && !hardResetThisCycle && !softResetTrigger.valid) {
      // this is fine, because fences are always the last instruction in the ROB (cannot insert anything else while it's present)
      fenceDetected := False

      softResetThisCycle := True
      softResetTrigger.push(lastCorrectId)
      currentSoftResetTrigger.push(lastCorrectId)
      newestAtSoftReset := newestIndex

      // set fetch PC to correct next PC (even if it was predicted as something else) and invalidate issuepipeline stages
      pipeline.issuePipeline.service[JumpService].jump(nextPc)

      // reset last speculative instruction
      lastSpeculativeCFInstruction.setIdle()

      // ensure entries between oldest and newest are invalidated -> new entry should only be pushed
      //     if rob/cdb updates are already there for invalid or stages got invalidated
      for (relative <- 0 until capacity) {
        val absolute = absoluteIndexForRelative(relative).resized
        val entry = robEntries(absolute)
        when(isValidAbsoluteIndex(absolute)) {
          when(relative > relativeIndexForAbsolute(lastCorrectId)) {
            entry.invalidated := True
          } otherwise {
            // if there are still valid control-flow speculation instructions, set the youngest one as the last
            pipeline.serviceOption[SpeculationService] foreach { spec =>
              when(spec.isSpeculativeCF(entry.registerMap)) {
                lastSpeculativeCFInstruction.push(absolute)
              }
            }
          }
        }
      }
      softFlushCounter := softFlushCounter + 1

      ret := True
    }
    ret
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

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    // TODO: the PSF update logic is probably way too complicated...
    val lsu = pipeline.service[LsuService]
    if (config.addressBasedPsf) {
      when(lsu.psfMisspeculation(cdbMessage.metadata)) {
        robEntries(cdbMessage.robIndex).cdbUpdated := robEntries(
          cdbMessage.robIndex
        ).rdbUpdated || (currentRdbUpdate.valid && currentRdbUpdate.payload === cdbMessage.robIndex)
        currentCdbUpdate.push(cdbMessage.robIndex)
        // do not update stored value when it's a misprediction update
        psfMispredictions := psfMispredictions + 1
        addPsfPredictorEntry(
          robEntries(cdbMessage.robIndex).registerMap
            .elementAs[UInt](pipeline.data.PC.asInstanceOf[PipelineData[Data]])
        )
        val flushed = softFlush(
          cdbMessage.robIndex,
          robEntries(cdbMessage.robIndex).registerMap.elementAs[UInt](
            pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
          )
        )
        currentCdbSoftReset := flushed
        when(!flushed) {
          pipeline
            .service[JumpService]
            .jumpOfBundle(robEntries(cdbMessage.robIndex).registerMap) := True
        }
      } otherwise {
        robEntries(cdbMessage.robIndex).cdbUpdated := True
        robEntries(cdbMessage.robIndex).registerMap
          .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
      }
    } else {
      robEntries(cdbMessage.robIndex).cdbUpdated := True
      robEntries(cdbMessage.robIndex).registerMap
        .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
    }

    lsu.psfAddress(robEntries(cdbMessage.robIndex).registerMap) := lsu.psfAddress(
      cdbMessage.metadata
    )

    pipeline.serviceOption[SpeculationService] foreach { spec =>
      // mark PSF speculation
      spec.isSpeculativeMD(robEntries(cdbMessage.robIndex).registerMap) := spec.isSpeculativeMD(
        cdbMessage.metadata
      )
      spec.isSpeculativeCF(robEntries(cdbMessage.robIndex).registerMap) := spec.isSpeculativeCF(
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
      val isYounger = relativeIndexForAbsolute(index) > relativeIndexForAbsolute(storeIndex)

      val speculative = pipeline.service[LsuService].stlSpeculation(entry.registerMap)

      val entriesMatch: Bool = if (config.addressBasedSsb) {
        isValidAbsoluteIndex(
          nth
        ) && entryIsLoad && isYounger && (entryAddressValid && addressesMatch && speculative)
      } else {
        isValidAbsoluteIndex(
          nth
        ) && entryIsLoad && isYounger && (entryAddressValid && addressesMatch && speculative) && (!valueValid || storeValue =/= loadValue)
      }

      when(entriesMatch) {
        found := True
      }
    }
    found
  }

  def onRdbMessage(rdbMessage: RdbMessage): Unit = {
    val btb = pipeline.service[BranchTargetPredictorService]
    val jmp = pipeline.service[JumpService]
    val lsu = pipeline.service[LsuService]

    currentRdbUpdate.push(rdbMessage.robIndex)
    robEntries(rdbMessage.robIndex).registerMap := rdbMessage.registerMap
    robEntries(rdbMessage.robIndex).willCdbUpdate := rdbMessage.willCdbUpdate

    when(
      rdbMessage.registerMap.elementAs[UInt](
        pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
      ) =/= btb.predictedPc(rdbMessage.registerMap)
    ) {
      val flushed = False
      when(!currentCdbSoftReset) {
        flushed := softFlush(
          rdbMessage.robIndex,
          rdbMessage.registerMap.elementAs[UInt](
            pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
          )
        )
      }

      when(flushed) {
        btb.preventFlush(robEntries(rdbMessage.robIndex).registerMap)
      }
    }

    when(pipeline.service[CsrService].isCsrInstruction(rdbMessage.registerMap)) {
      jmp.jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
      val flushed = False
      when(!currentCdbSoftReset) {
        flushed := softFlush(
          rdbMessage.robIndex,
          rdbMessage.registerMap.elementAs[UInt](
            pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
          )
        )
      }

      when(!flushed) {
        // CSR reads are not correctly handled, so for now we always flush
      }
    }

    if (config.stlSpec) {
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

        val ssbReset = hasSpeculatingLoad(rdbMessage.robIndex, storeValue, storeAddress)

        when(ssbReset) {
          addSsbPredictorEntry(
            rdbMessage.registerMap.elementAs[UInt](
              pipeline.data.PC.asInstanceOf[PipelineData[Data]]
            )
          )
          ssbMispredictions := ssbMispredictions + 1
          val flushed = False
          when(!currentCdbSoftReset) {
            flushed := softFlush(
              rdbMessage.robIndex,
              rdbMessage.registerMap.elementAs[UInt](
                pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
              )
            )
          }
          when(!flushed) {
            jmp.jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
          }
        }
      }

      if (config.addressBasedPsf) {
        when(lsu.psfMisspeculation(rdbMessage.registerMap)) {
          psfMispredictions := psfMispredictions + 1
          addPsfPredictorEntry(
            robEntries(rdbMessage.robIndex).registerMap
              .elementAs[UInt](pipeline.data.PC.asInstanceOf[PipelineData[Data]])
          )
          val flushed = softFlush(
            rdbMessage.robIndex,
            rdbMessage.registerMap.elementAs[UInt](
              pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
            )
          )
          when(!flushed) {
            jmp.jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
          }
        }
        when(
          lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.LOAD
        ) {
          when(
            (currentCdbUpdate.valid && currentCdbUpdate.payload === rdbMessage.robIndex) || jmp
              .jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap)
          ) {
            jmp.jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
          } otherwise {
            psfPredictions := psfPredictions + 1
          }
        }
      }

      if (!config.addressBasedPsf) {
        // detect wrongly forwarded value-based PSF
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
            addPsfPredictorEntry(
              robEntries(rdbMessage.robIndex).registerMap
                .elementAs[UInt](pipeline.data.PC.asInstanceOf[PipelineData[Data]])
            )
            val flushed = False
            when(!currentCdbSoftReset) {
              flushed := softFlush(
                rdbMessage.robIndex,
                rdbMessage.registerMap.elementAs[UInt](
                  pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
                )
              )
            }
            when(!flushed) {
              jmp.jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
            }
          } otherwise {
            psfPredictions := psfPredictions + 1
          }
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
