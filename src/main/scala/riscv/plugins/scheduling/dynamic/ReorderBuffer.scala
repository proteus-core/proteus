package riscv.plugins.scheduling.dynamic

import riscv.Config
import spinal.core._

object RobEntryType extends SpinalEnum {
  val BRANCH, STORE, REGISTER = newElement()
}

case class RobEntry(implicit config: Config) extends Bundle {
  val instructionType = RobEntryType()
  val destination = UInt(config.xlen bits)
  val value = UInt(config.xlen bits)
  val ready = Bool()
}

class ReorderBuffer(registerFile: Scheduler#RegisterFile, robCapacity: Int)(implicit config: Config) extends Area with CdbListener {
  def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry().getZero))
  val oldestIndex = Reg(UInt(indexBits)).init(0)
  val newestIndex = Reg(UInt(indexBits)).init(0)
  private val isFull = RegInit(False)
  private val willRetire = False
  val isAvailable = !isFull || willRetire

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry()
  pushedEntry := RobEntry().getZero

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

  def pushEntry(instructionType: SpinalEnumElement[RobEntryType.type], destination: UInt): UInt = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.instructionType := instructionType
    pushedEntry.destination := destination.resized
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
      when (isValidIndex(index) && entry.destination === regId) {
        found := True
        ready := entry.ready
        value := entry.value
        ix := index.resized
      }
    }
    (found, ready, value, ix)
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).value := cdbMessage.value
    robEntries(cdbMessage.robIndex).ready := True
  }

  def build(): Unit = {
    val oldestEntry = robEntries(oldestIndex)
    val updatedOldestIndex = UInt(indexBits)
    updatedOldestIndex := oldestIndex
    val isEmpty = oldestIndex === newestIndex && !isFull
    when (!isEmpty && oldestEntry.ready) {
      when (oldestEntry.instructionType === RobEntryType.REGISTER) {
        registerFile.updateValue(oldestEntry.destination.resized, oldestEntry.value)
      }
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
