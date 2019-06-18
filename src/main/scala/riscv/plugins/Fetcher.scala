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

      val ibus = slave(new MemBus(config.xlen))

      val pc = input(pipeline.data.NEXT_PC)
      val nextPc = pc + 4

      ibus.address := (pc >> 2).resized
      ibus.read := True
      ibus.write := False
      ibus.wdata := 0
      ibus.wmask.assignDontCare()
      output(pipeline.data.PC) := pc
      output(pipeline.data.NEXT_PC) := nextPc
      output(pipeline.data.IR) := ibus.rdata
    }

    val pipelineArea = pipeline plug new Area {
      val ibus = slave(new MemBus(config.xlen))
      ibus <> fetchArea.ibus
      Fetcher.this.ibus = ibus
    }

    pipelineArea.setName("")
  }
}
