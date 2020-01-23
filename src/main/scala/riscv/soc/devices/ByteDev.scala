package riscv.soc.devices

import riscv._
import riscv.soc._

import spinal.core._
import spinal.lib._

class ByteDevIo(implicit config: Config) extends Bundle with IMasterSlave {
  val wdata = Flow(UInt(8 bits))
  val rdata = Stream(UInt(8 bits))

  override def asMaster(): Unit = {
    master(wdata)
    slave(rdata)
  }
}

class ByteDev(implicit config: Config) extends MmioDevice {
  val io = master(new ByteDevIo)
  val irq = master(new IrqIo)
  irq.init()

  io.wdata.valid := False
  io.wdata.payload.assignDontCare()

  private val rbuf = Reg(UInt(8 bits)).init(0)
  private val rbufFull = Reg(Bool()).init(False)

  io.rdata.ready := !rbufFull

  when (io.rdata.valid && !rbufFull) {
    rbuf := io.rdata.payload
    rbufFull := True
    irq.postInterrupt()
  }

  addRegister(0, new MmioRegister {
    override val width: BitCount = config.xlen bits

    override def write(value: UInt): Unit = {
      io.wdata.push(value(7 downto 0))
    }

    override def read(): UInt = {
      val data = UInt(8 bits)

      when (rbufFull) {
        data := rbuf
      } elsewhen (io.rdata.valid) {
        data := io.rdata.payload
      } otherwise {
        data := 0
      }

      rbufFull := False
      irq.clearInterrupt()

      data.resized
    }
  })

  build()
}
