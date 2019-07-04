package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

import java.io.File

class Fetcher(implicit config: Config) extends Plugin with IBusService {
  private var ibus: MemBus = null

  override def getIBus: MemBus = {
    assert(ibus != null, "Call build() first")
    ibus
  }

  override def build(pipeline: Pipeline): Unit = {
    val fetchArea = pipeline.fetch plug new Area {
      import pipeline.fetch._

      val ibus = slave(new MemBus(config.ibusConfig))
      ibus.cmd.valid := False
      ibus.cmd.address.assignDontCare()

      // This ensures that stale requests are always dropped. If we only set
      // this to True when ibus.rsp.valid is True, it might happen that we just
      // started a new request and the response of the previous request is still
      // valid, causing us to use the stale response.
      ibus.rsp.ready := True

      arbitration.isReady := False

      val pc = input(pipeline.data.NEXT_PC)
      val nextPc = pc + 4

      when (arbitration.isRunning) {
        ibus.cmd.address := pc
        ibus.cmd.valid := True

        when (ibus.rsp.valid) {
          arbitration.isReady := True

          output(pipeline.data.PC) := pc
          output(pipeline.data.NEXT_PC) := nextPc
          output(pipeline.data.IR) := ibus.rsp.rdata
        }
      }
    }

    val pipelineArea = pipeline plug new Area {
      val ibus = slave(new MemBus(config.ibusConfig))
      ibus <> fetchArea.ibus
      Fetcher.this.ibus = ibus
    }

    pipelineArea.setName("")
  }
}
