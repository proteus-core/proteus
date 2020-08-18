package riscv.plugins.capabilities

import riscv._

import spinal.core._

class ExceptionHandler extends Plugin[Pipeline] with ExceptionService {
  override def handleException(stage:Stage, cause: ExceptionCause, reg: UInt): Unit = {
    assert(reg.getBitsWidth == 6)

    val trapHandler = pipeline.getService[TrapService]
    trapHandler.trap(stage, TrapCause.CheriException)
  }
}
