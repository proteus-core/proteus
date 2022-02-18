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
  private val isFull = RegInit(False)
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
    when ((oldestIndex.value === newestIndex.value && !isFull) || index >= capacity) { // initial setting
      ret := False
    } elsewhen (oldestIndex.value === newestIndex.value) { // rob is full
      ret := True
    } elsewhen (newestIndex.value > oldestIndex.value) { // normal order
      ret := index >= oldestIndex.value && index < newestIndex.value
    } otherwise { // wrapping
      ret := index >= oldestIndex.value || index < newestIndex.value
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
    absolute := (relative + oldestIndex.value).resized
    when (absolute >= capacity) {
      adjusted := (absolute - capacity).resized
    } otherwise {
      adjusted := absolute
    }
    adjusted
  }

  def pushEntry(rd: UInt, rdType: SpinalEnumCraft[RegisterType.type], lsuOperationType: SpinalEnumCraft[LsuOperationType.type], isBranch: Bool, pc: UInt): UInt = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.hasValue := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := pc
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := rd
    pushedEntry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := rdType
    pipeline.getService[LsuService].operationOfBundle(pushedEntry.registerMap) := lsuOperationType
    pipeline.getService[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False
    pipeline.getService[BranchService].isBranchOfBundle(pushedEntry.registerMap) := isBranch
    pipeline.withService[ContextService](
      context => {
        context.isSecretOfBundle(pushedEntry.registerMap) := False
      }
    )
    newestIndex.value
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).hasValue := True
    robEntries(cdbMessage.robIndex).registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
    pipeline.withService[ContextService](
      context => {
        context.isSecretOfBundle(pushedEntry.registerMap) := context.isSecretOfBundle(cdbMessage.metadata)
      }
    )
  }

  def getValue(regId: UInt): (Bool, Flow[CdbMessage]) = {
    val found = Bool()
    val target = Flow(CdbMessage(metaRegisters, indexBits))
    target.valid := False
    target.payload.robIndex := 0
    target.payload.writeValue := 0
    pipeline.withService[ContextService](
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
        pipeline.withService[ContextService](
          context => {
            context.isSecretOfBundle(target.metadata) := context.isSecretOfBundle(entry.registerMap)
          }
        )
      }
    }
    (found, target) // TODO: ready also in metadata?
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

  def isTransient(robIndex: UInt): Flow[UInt] = {
    val dependent = Flow(UInt(indexBits))
    dependent.valid := False
    dependent.payload := 0

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val isOlder = relativeIndexForAbsolute(index) < relativeIndexForAbsolute(robIndex)
      val olderIsBranch = pipeline.getService[BranchService].isBranchOfBundle(entry.registerMap) // || isPredicted(entry.registerMap)
      val olderIsNotResolved = !entry.ready
      val incorrectlyPredicted = True

      when (isValidAbsoluteIndex(nth)
        && isOlder
        && olderIsBranch
        && (olderIsNotResolved || incorrectlyPredicted)) {
        dependent.push(index)
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

  def clearSecretFlag(): Unit = {
    val secondBranch = Bool()
    secondBranch := False
    for (nth <- 1 until capacity) {
      val absolute = absoluteIndexForRelative(nth).resized
      val entry = robEntries(absolute)
      when (pipeline.getService[BranchService].isBranchOfBundle(entry.registerMap)) {
        secondBranch := True
      }
      when (!secondBranch) {
        pipeline.getService[ContextService].isSecretOfBundle(entry.registerMap) := False
      }
    }
  }

  def build(): Unit = {
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
      // removing secret flags, we don't care whether the prediction was correct or not, if not, a flush will happen in the next cycle anyway
      // TODO: should we only do this for jumps? it doesn't hurt like this, right?
      if (pipeline.hasService[ContextService]) {
        clearSecretFlag()
      }

      // removing the oldest entry
      updatedOldestIndex := oldestIndex.valueNext
      oldestIndex.increment()
      willRetire := True
      isFull := False
    }

    when (pushInCycle) {
      robEntries(newestIndex.value) := pushedEntry
      val updatedNewest = newestIndex.valueNext
      newestIndex.increment()
      when (updatedOldestIndex === updatedNewest) {
        isFull := True
      }
    }
  }
}
