package riscv.plugins.capabilities

import riscv._

import spinal.core._

trait PccService {
  def getPcc(stage: Stage): Capability
  def jump(stage: Stage, pcc: Capability, capIdx: CapIdx): Unit
}

trait Scr {
  val needAsr: Boolean

  def read(): Capability
  def write(value: Capability)
}

trait ScrService {
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability

  def registerScr[T <: Scr](id: Int, scr: => T): T

  /**
    * Register a new SCR that extends the given CSR. Extending a CSR means that
    * reads and write of the CSR will refer to this SCR's offset.
    */
  def registerScr[T <: Scr](id: Int, offsetCsr: Int, scr: => T): T
  def getScr(stage: Stage, id: Int): Scr
}

trait ExceptionService {
  protected def handleException(stage: Stage, cause: UInt, capIdx: CapIdx): Unit

  def except(stage: Stage, cause: UInt, capIdx: CapIdx): Unit = {
    handleException(stage, cause, capIdx)
  }

  def except(stage: Stage, cause: ExceptionCause, capIdx: CapIdx): Unit = {
    handleException(stage, U(cause.code), capIdx)
  }
}

trait TaggedMemoryService {
  def createCapBus(stage: Stage): CapBus
}
