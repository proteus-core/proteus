package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._

case class RdbMessage(retirementRegisters: DynBundle[PipelineData[Data]],
                      robIndexBits: BitCount) extends Bundle {
  val robIndex = UInt(robIndexBits)
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] = retirementRegisters.createBundle
}

case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]])
                   (implicit config: Config) extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] = retirementRegisters.createBundle
  val ready = Bool()

  override def clone(): RobEntry = {
    RobEntry(retirementRegisters)
  }
}

class ReorderBuffer(pipeline: DynamicPipeline,
                    robCapacity: Int,
                    retirementRegisters: DynBundle[PipelineData[Data]])
                   (implicit config: Config) extends Area {
  def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry(retirementRegisters).getZero))
  val oldestIndex = Reg(UInt(indexBits)).init(0)
  val newestIndex = Reg(UInt(indexBits)).init(0) // TODO: use built-in counter class for these?
  private val isFull = RegInit(False)
  private val willRetire = False
  val isAvailable = !isFull || willRetire

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry(retirementRegisters)
  pushedEntry := RobEntry(retirementRegisters).getZero

  def reset(): Unit = {
    oldestIndex := 0
    newestIndex := 0
    isFull := False
  }

  def nextIndex(index: UInt): UInt = {
    val next = UInt()
    when (index + 1 === capacity) {
      next := 0
    } otherwise {
      next := index + 1
    }
    next
  }

  def isValidIndex(index: UInt): Bool = {
    val ret = Bool()
    when ((oldestIndex === newestIndex && !isFull) || index >= capacity) { // initial setting
      ret := False
    } elsewhen (oldestIndex === newestIndex) { // rob is full
      ret := True
    } elsewhen (newestIndex > oldestIndex) { // normal order
      ret := index >= oldestIndex && index < newestIndex
    } otherwise { // wrapping
      ret := index >= oldestIndex || index < newestIndex
    }
    ret
  }

  def isOlderThan(first: UInt, second: UInt): Bool = {
    val ret = Bool()
    when (!isValidIndex(first) || !isValidIndex(second)) {
      ret := False
    } elsewhen (first < second && !(oldestIndex >= first && oldestIndex <= second)) {
      ret := True
    } elsewhen (first > second && (oldestIndex <= first && oldestIndex > second)) {
      ret := True
    } otherwise {
      ret := False
    }
    ret
  }

  def indexForNth(nth: UInt): UInt = {
    val index = UInt(config.xlen bits)
    val adjusted = UInt(config.xlen bits)
    index := (nth + oldestIndex).resized
    when (index >= capacity) {
      adjusted := index - capacity
    } otherwise {
      adjusted := index
    }
    adjusted
  }

  def pushEntry(rd: UInt, lsuOperationType: SpinalEnumCraft[LsuOperationType.type]): UInt = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := rd
    pipeline.getService[LsuService].operationOfBundle(pushedEntry.registerMap) := lsuOperationType
    pipeline.getService[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False
    newestIndex
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
    for (nth <- 0 until capacity) {
      val index = indexForNth(nth)
      val entry = robEntries(index.resized)

      // last condition: prevent dependencies on x0
      when (isValidIndex(index)
        && entry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) === regId  // TODO: this assumes that non-RD instructions have RD == 0 (or an invalid value)
        && regId =/= 0) {
        found := True
        ready := entry.ready
        value := entry.registerMap.elementAs[UInt](pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])
        ix := index.resized
      }
    }
    (found, ready, value, ix)
  }

  def hasPendingStore(robIndex: UInt, address: UInt): Bool = {
    val found = Bool()
    found := False

    for (nth <- 0 until capacity) {
      val index = indexForNth(nth)
      val entry = robEntries(index.resized)

      when (isValidIndex(index)
        && isOlderThan(index, robIndex)
        && pipeline.getService[LsuService].operationOfBundle(entry.registerMap) === LsuOperationType.STORE
        && ((pipeline.getService[LsuService].addressValidOfBundle(entry.registerMap) === True
            && pipeline.getService[LsuService].addressOfBundle(entry.registerMap) === address)
          || pipeline.getService[LsuService].addressValidOfBundle(entry.registerMap) === False)) {
        found := True
      }
    }
    found
  }

  def onRdbMessage(rdbMessage: RdbMessage): Unit = {
    robEntries(rdbMessage.robIndex).registerMap := rdbMessage.registerMap
    robEntries(rdbMessage.robIndex).ready := True
  }

  def build(): Unit = {
    val oldestEntry = robEntries(oldestIndex)
    val updatedOldestIndex = UInt(indexBits)
    updatedOldestIndex := oldestIndex
    val isEmpty = oldestIndex === newestIndex && !isFull

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

      // removing the oldest entry
      updatedOldestIndex := nextIndex(oldestIndex)
      oldestIndex := updatedOldestIndex
      willRetire := True
      isFull := False
    }

    when (pushInCycle) {
      robEntries(newestIndex) := pushedEntry
      val updatedNewest = nextIndex(newestIndex)
      newestIndex := updatedNewest
      when (updatedOldestIndex === updatedNewest) {
        isFull := True
      }
    }
  }
}
