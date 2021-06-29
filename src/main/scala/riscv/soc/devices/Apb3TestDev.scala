package riscv.soc.devices

import riscv._
import riscv.soc._

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

class Apb3TestDev(implicit config: Config) extends Component{
  val io = new Bundle {
    val test = master(Flow(UInt(config.xlen bits)))
    val apb = slave(Apb3(Apb3Config(addressWidth = 4, dataWidth = config.xlen)))
  }

  val busCtrl = Apb3SlaveFactory(io.apb)

  busCtrl.driveFlow(io.test, 0)
}
