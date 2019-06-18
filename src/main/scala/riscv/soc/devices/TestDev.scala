package riscv.soc.devices

import riscv._
import riscv.soc._

import spinal.core._
import spinal.lib._

class TestDev(implicit config: Config) extends MmioDevice {
  val io = master(Flow(UInt(config.xlen bits)))
  io.valid := False
  io.payload.assignDontCare()

  addRegister(0, new MmioRegister {
    override val width: BitCount = 32 bits

    override def write(value: UInt): Unit = {
      io.push(value)
    }
  })

  build()
}
