package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._

case class InstructionActions() extends Bundle {
  val writesRegister = Bool()
  val performsJump = Bool()
}

object InstructionActions {
  def none(): InstructionActions = {
    val actions = InstructionActions()
    actions.writesRegister := False
    actions.performsJump := False
    actions
  }
}

// TODO: revisit how these signals are used for different instruction types
case class RobEntry(implicit config: Config) extends Bundle {
  val actions = InstructionActions()
  val writeDestination = UInt(config.xlen bits)
  val writeValue = UInt(config.xlen bits)
  val actualJumpTarget = UInt(config.xlen bits)
  val predictedJumpTarget = UInt(config.xlen bits)
  val ready = Bool()
}

class ReorderBuffer(pipeline: DynamicPipeline,
                    registerFile: Scheduler#RegisterFile,
                    robCapacity: Int)
                   (implicit config: Config) extends Area with CdbListener {
  def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry().getZero))
  val oldestIndex = Reg(UInt(indexBits)).init(0)
  val newestIndex = Reg(UInt(indexBits)).init(0) // TODO: use built-in counter class for these?
  private val isFull = RegInit(False)
  private val willRetire = False
  val isAvailable = !isFull || willRetire

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry()
  pushedEntry := RobEntry().getZero

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

  def pushEntry(destination: UInt): UInt = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.actions := InstructionActions.none
    pushedEntry.writeDestination := destination.resized
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
      when (isValidIndex(index) && entry.writeDestination === regId && regId =/= 0) {
        found := True
        ready := entry.ready
        value := entry.writeValue
        ix := index.resized
      }
    }
    (found, ready, value, ix)
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).writeValue := cdbMessage.writeValue
    robEntries(cdbMessage.robIndex).actions := cdbMessage.actions
    robEntries(cdbMessage.robIndex).actualJumpTarget := cdbMessage.actualJumpTarget
    robEntries(cdbMessage.robIndex).predictedJumpTarget := cdbMessage.predictedJumpTarget
    robEntries(cdbMessage.robIndex).ready := True
  }

  def build(): Unit = {
    val oldestEntry = robEntries(oldestIndex)
    val updatedOldestIndex = UInt(indexBits)
    updatedOldestIndex := oldestIndex
    val isEmpty = oldestIndex === newestIndex && !isFull

    // TODO: is the performsJump flag even necessary?
    when (!isEmpty && oldestEntry.ready) {
      when (oldestEntry.actualJumpTarget =/= oldestEntry.predictedJumpTarget) {
        val jumpService = pipeline.getService[JumpService]
        jumpService.jump(oldestEntry.actualJumpTarget)
      }

      when (oldestEntry.actions.writesRegister) {
        registerFile.updateValue(oldestEntry.writeDestination.resized, oldestEntry.writeValue)
      }

      // removing the oldest entry and potentially resetting the ROB in case of a jump
      when (oldestEntry.actualJumpTarget =/= oldestEntry.predictedJumpTarget) {
        reset()
      } otherwise {
        updatedOldestIndex := nextIndex(oldestIndex)
        oldestIndex := updatedOldestIndex
        willRetire := True
        isFull := False
      }
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
