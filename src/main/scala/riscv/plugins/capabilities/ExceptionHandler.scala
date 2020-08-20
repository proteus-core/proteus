package riscv.plugins.capabilities

import riscv._

import spinal.core._
import spinal.lib._

class ExceptionHandler extends Plugin[Pipeline] with ExceptionService {
  object Data {
    object CEXC_CAUSE extends PipelineData(UInt(5 bits))
    object CEXC_CAP_IDX extends PipelineData(UInt(6 bits))
  }

  override def handleException(stage: Stage, cause: ExceptionCause, reg: UInt): Unit = {
    assert(reg.getBitsWidth == 6)

    val trapHandler = pipeline.getService[TrapService]
    trapHandler.trap(stage, TrapCause.CheriException)
    stage.output(Data.CEXC_CAUSE) := cause.code
    stage.output(Data.CEXC_CAP_IDX) := reg
  }

  override def setup(): Unit = {
    val trapHandler = pipeline.getService[TrapService]
    val csrFile = pipeline.getService[CsrService]

    trapHandler.onTrapCommit {(stage, isInterrupt, cause) =>
      val ccsr = slave(new CsrIo)

      pipeline plug {
        ccsr <> csrFile.getCsr(0xbc0)
      }

      when (!isInterrupt && cause === TrapCause.CheriException.code) {
        val newCcsr = UInt(config.xlen bits)
        newCcsr := ccsr.read()
        newCcsr(9 downto 5) := stage.value(Data.CEXC_CAUSE)
        newCcsr(15 downto 10) := stage.value(Data.CEXC_CAP_IDX)
        ccsr.write(newCcsr)
      }
    }
  }
}
