package riscv.plugins.capabilities

import riscv._

import spinal.core._

trait ScrService {
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability
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
