package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Counter

case class RdbMessage(retirementRegisters: DynBundle[PipelineData[Data]],
                      robIndexBits: BitCount) extends Bundle {
  val robIndex = UInt(robIndexBits)
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] = retirementRegisters.createBundle
}

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
                    retirementRegisters: DynBundle[PipelineData[Data]])
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

  def pushEntry(rd: UInt, rdType: SpinalEnumCraft[RegisterType.type], lsuOperationType: SpinalEnumCraft[LsuOperationType.type], pc: UInt): UInt = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.hasValue := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := pc
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := rd
    pushedEntry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := rdType
    pipeline.getService[LsuService].operationOfBundle(pushedEntry.registerMap) := lsuOperationType
    pipeline.getService[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False
    newestIndex.value
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).hasValue := True
    robEntries(cdbMessage.robIndex).registerMap.element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
  }

  def getValue(regId: UInt): (Bool, Bool, UInt, UInt) = {
    val found = Bool()
    val ready = Bool()
    val value = UInt(config.xlen bits)
    val ix = UInt(indexBits)
    found := False
    ready := False
    value := 0
    ix := 0

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
        ready := entry.hasValue
        value := entry.registerMap.elementAs[UInt](pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])
        ix := absolute
      }
    }
    (found, ready, value, ix)
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

  def onRdbMessage(rdbMessage: RdbMessage): Unit = {
    robEntries(rdbMessage.robIndex).registerMap := rdbMessage.registerMap

    when (pipeline.getService[CsrService].isCsrInstruction(rdbMessage.registerMap)) {
      pipeline.getService[JumpService].jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
    }

    robEntries(rdbMessage.robIndex).ready := True
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
