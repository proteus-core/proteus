package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

class Fetcher(implicit config: Config) extends Plugin with IBusService {
  private var ibus: MemBus = null

  override def getIBus: MemBus = {
    assert(ibus != null, "Call build() first")
    ibus
  }

  override def build(pipeline: Pipeline): Unit = {
    val fetchArea = pipeline.fetch plug new Area {
      import pipeline.fetch._

      val ibus = master(new MemBus(config.ibusConfig))
      val ibusCtrl = new MemBusControl(ibus)

      arbitration.isReady := False

      val pc = input(pipeline.data.NEXT_PC)
      val nextPc = pc + 4

      when (arbitration.isRunning) {
        val (valid, rdata) = ibusCtrl.read(pc)

        when (valid) {
          arbitration.isReady := True

          output(pipeline.data.PC) := pc
          output(pipeline.data.NEXT_PC) := nextPc
          output(pipeline.data.IR) := rdata
        }
      }
    }

    val pipelineArea = pipeline plug new Area {
      val ibus = master(new MemBus(config.ibusConfig))
      ibus <> fetchArea.ibus
      Fetcher.this.ibus = ibus
    }

    pipelineArea.setName("")
  }
}
