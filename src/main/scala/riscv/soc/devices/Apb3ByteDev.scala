package riscv.soc.devices

import riscv._

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

class ByteDevIo(implicit config: Config) extends Bundle with IMasterSlave {
  val wdata = Flow(UInt(8 bits))
  val rdata = Stream(UInt(8 bits))

  override def asMaster(): Unit = {
    master(wdata)
    slave(rdata)
  }
}

class Apb3ByteDev(implicit config: Config) extends Component {
  val io = new Bundle {
    val bytes = master(new ByteDevIo)
    val irq = master(new IrqIo)
    val apb = slave(Apb3(Apb3Config(addressWidth = 4, dataWidth = config.xlen)))
  }

  io.irq.init()

  private val rbuf = Reg(UInt(8 bits)).init(0)
  private val rbufFull = Reg(Bool()).init(False)

  io.bytes.rdata.ready := !rbufFull

  when(io.bytes.rdata.valid && !rbufFull) {
    rbuf := io.bytes.rdata.payload
    rbufFull := True
    io.irq.postInterrupt()
  }

  private val busCtrl = Apb3SlaveFactory(io.apb)
  busCtrl.driveFlow(io.bytes.wdata, 0)

  private val rdata = U(0, 8 bits)

  when(rbufFull) {
    rdata := rbuf
  } elsewhen (io.bytes.rdata.valid) {
    rdata := io.bytes.rdata.payload
  }

  busCtrl.read(rdata, 0)

  busCtrl.onRead(0) {
    rbufFull := False
    io.irq.clearInterrupt()
  }
}
