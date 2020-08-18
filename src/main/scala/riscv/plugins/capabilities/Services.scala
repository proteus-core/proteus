package riscv.plugins.capabilities

import riscv._

import spinal.core._

trait ScrService {
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability
}

trait ExceptionService {
  protected def handleException(stage:Stage, cause: ExceptionCause, reg: UInt)

  def except(stage:Stage, cause: ExceptionCause, cr: UInt): Unit = {
    assert(cr.getBitsWidth <= 5)
    handleException(stage, cause, cr.resize(6 bits))
  }

  def except(stage:Stage, cause: ExceptionCause, scr: Int): Unit = {
    val reg = Bits(6 bits)
    reg.msb := True
    reg(5 downto 0) := scr
    handleException(stage, cause, reg.asUInt)
  }
}
