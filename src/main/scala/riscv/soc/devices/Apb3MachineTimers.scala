package riscv.soc.devices

import riscv._

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

class Apb3MachineTimers(pipeline: Pipeline)(implicit config: Config) extends Component {
  val io = new Bundle {
    val mtimer = master(new IrqIo)
    val apb = slave(Apb3(Apb3Config(addressWidth = 4, dataWidth = config.xlen)))
  }

  io.mtimer.init()

  private val mtime = Reg(UInt(64 bits)).init(0)
  mtime := mtime + 1

  private val mtimecmp = Reg(UInt(64 bits)).init(0)

  parent rework {
    pipeline.serviceOption[InterruptService] foreach {interruptService =>
      interruptService.getMachineTimerIrqIo <> io.mtimer
    }
  }

  // It is important that this comes before the call to build() to ensure that
  // the clearing of the mtip bit on a write has precedence over setting it.
  when (mtime >= mtimecmp) {
    io.mtimer.postInterrupt()
  }

  private val busCtrl = Apb3SlaveFactory(io.apb)

  busCtrl.readAndWriteMultiWord(mtime, 0)
  busCtrl.readAndWriteMultiWord(mtimecmp, 8)

  busCtrl.onWrite(8) {
    io.mtimer.clearInterrupt()
  }
}
