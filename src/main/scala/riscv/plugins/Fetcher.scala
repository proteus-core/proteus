package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

class Fetcher(fetchStage: Stage) extends Plugin[Pipeline] {
  override def build(): Unit = {
    fetchStage plug new Area {
      import fetchStage._

      val ibus = pipeline.getService[MemoryService].createInternalIBus(fetchStage)
      val ibusCtrl = new MemBusControl(ibus)

      arbitration.isReady := False

      val pc = input(pipeline.data.PC)
      val nextPc = pc + 4

      when (arbitration.isRunning) {
        val (valid, rdata) = ibusCtrl.read(pc)

        when (valid) {
          arbitration.isReady := True

          output(pipeline.data.NEXT_PC) := nextPc
          output(pipeline.data.IR) := rdata
        }
      }
    }
  }
}
