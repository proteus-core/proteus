package riscv.plugins.capabilities

import riscv._

import spinal.core._

trait ScrService {
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability
}

trait ExceptionService {
  protected def handleException(stage: Stage, cause: UInt, reg: UInt): Unit

  def except(stage: Stage, cause: UInt, cr: UInt): Unit = {
    assert(cr.getBitsWidth <= 5)
    handleException(stage, cause, cr.resize(6 bits))
  }

  def except(stage: Stage, cause: UInt, scr: Int): Unit = {
    val reg = Bits(6 bits)
    reg.msb := True
    reg(4 downto 0) := scr
    handleException(stage, cause, reg.asUInt)
  }

  def except(stage: Stage, cause: ExceptionCause, cr: UInt): Unit = {
    except(stage, U(cause.code), cr)
  }

  def except(stage: Stage, cause: ExceptionCause, scr: Int): Unit = {
    except(stage, U(cause.code), scr)
  }
}

trait TaggedMemoryService {
  def createCapBus(stage: Stage): CapBus
}
