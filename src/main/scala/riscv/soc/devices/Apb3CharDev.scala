package riscv.soc.devices

import riscv._

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

class Apb3CharDev(implicit config: Config) extends Component{
  val io = new Bundle {
    val char = master(Flow(UInt(8 bits)))
    val apb = slave(Apb3(Apb3Config(addressWidth = 4, dataWidth = config.xlen)))
  }

  val busCtrl = Apb3SlaveFactory(io.apb)

  busCtrl.driveFlow(io.char, 0)
}
