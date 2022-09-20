package riscv.plugins.cheri

import riscv._

import spinal.core._
import spinal.lib._

class ExceptionHandler extends Plugin[Pipeline] with ExceptionService {
  object Data {
    object CEXC_CAUSE extends PipelineData(UInt(5 bits))
    object CEXC_CAP_IDX extends PipelineData(CapIdx())
  }

  override def handleException(stage: Stage, cause: UInt, capIdx: CapIdx): Unit = {
    assert(cause.getBitsWidth <= 5)

    val trapHandler = pipeline.service[TrapService]
    trapHandler.trap(stage, TrapCause.CheriException)
    stage.output(Data.CEXC_CAUSE) := cause.resized
    stage.output(Data.CEXC_CAP_IDX) := capIdx
  }

  override def setup(): Unit = {
    val trapHandler = pipeline.service[TrapService]
    val csrFile = pipeline.service[CsrService]

    trapHandler.onTrapCommit { (stage, isInterrupt, cause) =>
      val ccsr = slave(new CsrIo)

      pipeline plug {
        ccsr <> csrFile.getCsr(0xbc0)
      }

      when(!isInterrupt && cause === TrapCause.CheriException.code) {
        val newCcsr = UInt(config.xlen bits)
        newCcsr := ccsr.read()
        newCcsr(9 downto 5) := stage.value(Data.CEXC_CAUSE)
        newCcsr(15 downto 10) := stage.value(Data.CEXC_CAP_IDX).asUInt
        ccsr.write(newCcsr)
      }
    }
  }
}
