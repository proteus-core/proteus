package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.{Counter, Flow}

case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]])
                   (implicit config: Config) extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] = retirementRegisters.createBundle
  val ready = Bool()
  val hasValue = Bool()

  override def clone(): RobEntry = {
    RobEntry(retirementRegisters)
  }
}

/**
  * Terminology:
  * - absolute index: the actual index of an entry in the circular buffer
  * - relative index: an index that shows the order of instructions inserted into the ROB, 0 being
  *                   the oldest
  *
  * Relative indices are internal to the ROB, outside components only see the absolute index of
  * an entry
  */
class ReorderBuffer(pipeline: DynamicPipeline,
                    robCapacity: Int,
                    retirementRegisters: DynBundle[PipelineData[Data]],
                    metaRegisters: DynBundle[PipelineData[Data]])
                   (implicit config: Config) extends Area with CdbListener {
  def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry(retirementRegisters).getZero))
  val oldestIndex = Counter(capacity)
  val newestIndex = Counter(capacity)
  private val isFullNext = Bool()
  private val isFull = RegNext(isFullNext).init(False)
  private val willRetire = False
  val isAvailable = !isFull || willRetire

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry(retirementRegisters)
  pushedEntry := RobEntry(retirementRegisters).getZero

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    isFull := False
  }

  def isValidAbsoluteIndex(index: UInt): Bool = {
    val ret = Bool()

    val oldest = UInt(indexBits)
    val newest = UInt(indexBits)
    oldest := oldestIndex.value
    newest := newestIndex.value

    when (index >= capacity) {
      ret := False
    } elsewhen (isFullNext) {
      ret := True
    } elsewhen (oldest === newest && !isFull) {  // empty
      ret := False
    } elsewhen (newest > oldest) { // normal order
      ret := index >= oldest && index < newest
    } otherwise { // wrapping
      ret := index >= oldest || index < newest
    }
    ret
  }

  def relativeIndexForAbsolute(absolute: UInt): UInt = {
    val adjustedIndex = UInt(32 bits)
    when (absolute >= oldestIndex.value) {
      adjustedIndex := (absolute.resized - oldestIndex.value).resized
    } otherwise {
      val remainder = capacity - oldestIndex.value
      adjustedIndex := (absolute + remainder).resized
    }
    adjustedIndex
  }

  def absoluteIndexForRelative(relative: UInt): UInt = {
    val absolute = UInt(32 bits)
    val adjusted = UInt(32 bits)
    val oldestResized = UInt(32 bits)
    oldestResized := oldestIndex.value.resized
    absolute := oldestResized + relative
    when (absolute >= capacity) {
      adjusted := absolute - capacity
    } otherwise {
      adjusted := absolute
    }
    adjusted
  }

  def pushEntry(rd: UInt, rdType: SpinalEnumCraft[RegisterType.type], lsuOperationType: SpinalEnumCraft[LsuOperationType.type], isBranch: Bool, pc: UInt, predictedPc: UInt): UInt = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.hasValue := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := pc
    pipeline.getService[BranchTargetPredictorService].predictedPcOfBundle(pushedEntry.registerMap) := predictedPc
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := rd
    pushedEntry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := rdType
    pipeline.getService[LsuService].operationOfBundle(pushedEntry.registerMap) := lsuOperationType
    pipeline.getService[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False
    pipeline.withService[ProspectService](
      context => {
        pipeline.getService[BranchService].isBranchOfBundle(pushedEntry.registerMap) := isBranch
        context.isSecretOfBundle(pushedEntry.registerMap) := False
      }
    )
    newestIndex.value
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).hasValue := True
    robEntries(cdbMessage.robIndex).registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
    robEntries(cdbMessage.robIndex).registerMap.element(pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]) := cdbMessage.metadata.elementAs[UInt](pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]])
    pipeline.withService[ProspectService](
      context => {
        context.isSecretOfBundle(robEntries(cdbMessage.robIndex).registerMap) := context.isSecretOfBundle(cdbMessage.metadata)
      }
    )
  }

  def findRegisterValue(regId: UInt): (Bool, Flow[CdbMessage]) = {
    val found = Bool()
    val target = Flow(CdbMessage(metaRegisters, indexBits))
    target.valid := False
    target.payload.robIndex := 0
    target.payload.writeValue := 0
    pipeline.withService[ProspectService](
      context => {
        context.isSecretOfBundle(target.metadata) := False
      }
    )
    found := False

    // loop through valid values and return the freshest if present
    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized
      val entry = robEntries(absolute)

      // last condition: prevent dependencies on x0
      when (isValidAbsoluteIndex(absolute)
        && entry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) === regId
        && regId =/= 0
        && entry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) === RegisterType.GPR) {
        found := True
        target.valid := entry.hasValue
        target.robIndex := absolute
        target.writeValue := entry.registerMap.elementAs[UInt](pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])
        pipeline.withService[ProspectService](
          context => {
            context.isSecretOfBundle(target.metadata) := context.isSecretOfBundle(entry.registerMap)
          }
        )
      }
    }
    (found, target)
  }

  def hasPendingStore(robIndex: UInt, address: UInt): Bool = {
    val found = Bool()
    found := False

    val wordAddress = config.dbusConfig.byte2WordAddress(address)

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val lsuService = pipeline.getService[LsuService]
      val entryIsStore = lsuService.operationOfBundle(entry.registerMap) === LsuOperationType.STORE
      val entryAddressValid = lsuService.addressValidOfBundle(entry.registerMap)
      val entryAddress = lsuService.addressOfBundle(entry.registerMap)
      val entryWordAddress = config.dbusConfig.byte2WordAddress(entryAddress)
      val addressesMatch = entryWordAddress === wordAddress
      val isOlder = relativeIndexForAbsolute(index) < relativeIndexForAbsolute(robIndex)

      when (isValidAbsoluteIndex(nth)
        && isOlder
        && entryIsStore
        && ((entryAddressValid && addressesMatch) || !entryAddressValid)) {
        found := True
      }
    }
    found
  }

  def hasUnresolvedBranch(robIndex: UInt): Flow[UInt] = {
    val dependent = Flow(UInt(indexBits))
    dependent.valid := False
    dependent.payload := 0

    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized
      val entry = robEntries(absolute)

      val isBranch = pipeline.getService[BranchService].isBranchOfBundle(entry.registerMap)
      val resolved = entry.hasValue
      val incorrectlyPredicted = pipeline.getService[BranchTargetPredictorService].predictedPcOfBundle(entry.registerMap) =/= entry.registerMap.elementAs[UInt](pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]])
      // TODO: since here we already know whether it was mispredicted, might as well reset the pipeline?

      // an instruction is transient if there is an instruction before it in the ROB which is either
      // an unresolved branch, or a resolved instruction (of any type) that was mispredicted
      when (isValidAbsoluteIndex(absolute)
        && absolute =/= robIndex  // TODO? can also be solved by clearing the ROB registers on reset? also, think twice about this: must be able to differnetiate
        && ((isBranch && !resolved) || (incorrectlyPredicted && resolved))) {
        dependent.push(absolute)
      }
    }
    dependent
  }

  def onRdbMessage(rdbMessage: RdbMessage): Unit = {
    robEntries(rdbMessage.robIndex).registerMap := rdbMessage.registerMap

    when (pipeline.getService[CsrService].isCsrInstruction(rdbMessage.registerMap)) {
      pipeline.getService[JumpService].jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
    }

    robEntries(rdbMessage.robIndex).ready := True
  }

  def taintRegister(context: ProspectService, entry: RobEntry): Unit = {
    val regId = entry.registerMap.elementAs[UInt](pipeline.data.RD.asInstanceOf[PipelineData[Data]])
    val secret = context.isSecretOfBundle(entry.registerMap)
    when (regId =/= 0
      && entry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) === RegisterType.GPR) {
      context.setSecretRegister(regId, secret)
    }
  }

  def build(): Unit = {
    isFullNext := isFull

    val oldestEntry = robEntries(oldestIndex.value)
    val updatedOldestIndex = UInt(indexBits)
    updatedOldestIndex := oldestIndex.value
    val isEmpty = oldestIndex.value === newestIndex.value && !isFull

    val ret = pipeline.retirementStage
    ret.arbitration.isValid := False
    ret.arbitration.isStalled := False

    for (register <- retirementRegisters.keys) {
      ret.input(register) := oldestEntry.registerMap.element(register)
    }

    // FIXME this doesn't seem the correct place to do this...
    ret.connectOutputDefaults()
    ret.connectLastValues()

    when (!isEmpty && oldestEntry.ready) {
      ret.arbitration.isValid := True
    }

    when (!isEmpty && oldestEntry.ready && ret.arbitration.isDone) {
      pipeline.withService[ProspectService](
        context => taintRegister(context, oldestEntry)
      )

      // removing the oldest entry
      updatedOldestIndex := oldestIndex.valueNext
      oldestIndex.increment()
      willRetire := True
      isFullNext := False
    }

    when (pushInCycle) {
      robEntries(newestIndex.value) := pushedEntry
      val updatedNewest = newestIndex.valueNext
      newestIndex.increment()
      when (updatedOldestIndex === updatedNewest) {
        isFullNext := True
      }
    }
  }
}
