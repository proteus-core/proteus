package riscv.sim

import riscv.soc.devices._

import spinal.core.sim._

class StdioByteDev(io: ByteDevIo) {
  private var currentStdinByte: Option[Byte] = None

  private def rdataTrigger: Seq[Any] = {
    Seq(currentStdinByte, io.rdata.ready.toBoolean)
  }

  io.rdata.valid #= false

  forkSensitive(rdataTrigger) {
    currentStdinByte.foreach { byte =>
      io.rdata.valid #= true
      io.rdata.payload #= byte

      if (io.rdata.ready.toBoolean) {
        currentStdinByte = None
      }
    }
  }

  def eval(): Unit = {
    io.rdata.valid #= false

    if (io.wdata.valid.toBoolean) {
      val byte = io.wdata.payload.toInt.toByte
      System.out.write(byte)
    }

    if (currentStdinByte.isEmpty && System.in.available() > 0) {
      currentStdinByte = Some(System.in.read().toByte)
    }
  }
}
