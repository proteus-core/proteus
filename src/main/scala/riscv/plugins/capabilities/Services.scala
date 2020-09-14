package riscv.plugins.capabilities

import riscv._

import spinal.core._

trait ScrService {
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability
}

sealed trait CapIdx {
  def idx: UInt
}

final case class ScrCapIdx(scr: Int) extends CapIdx {
  override def idx: UInt = {
    val reg = Bits(6 bits)
    reg.msb := True
    reg(4 downto 0) := scr
    reg.asUInt
  }
}

final case class GpcrCapIdx(cr: UInt) extends CapIdx {
  assert(cr.getBitsWidth <= 5)

  override def idx: UInt = {
    cr.resize(6 bits)
  }
}

trait ExceptionService {
  protected def handleException(stage: Stage, cause: UInt, reg: UInt): Unit

  def except(stage: Stage, cause: UInt, capIdx: CapIdx): Unit = {
    handleException(stage, cause, capIdx.idx)
  }

  def except(stage: Stage, cause: ExceptionCause, capIdx: CapIdx): Unit = {
    handleException(stage, U(cause.code), capIdx.idx)
  }
}

trait TaggedMemoryService {
  def createCapBus(stage: Stage): CapBus
}
