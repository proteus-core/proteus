package riscv

import riscv.plugins._

import java.io.File

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.HexTools

class Core(imemHexPath: String, formal: Boolean = false) extends Component {

  implicit val config = new Config(BaseIsa.RV32I)
  val pipelinePlugins = if (true) Seq(new SimplePipelining, new DataHazardResolver) else Seq(new NoPipelining)
  val extraPlugins = if (formal) Seq(new RiscvFormal) else Seq()

  var plugins = pipelinePlugins ++ Seq(
    new Fetcher,
    new Decoder,
    new RegisterFile,
    new IntAlu,
    new Lsu
  ) ++ extraPlugins

  val pipeline = new Pipeline(config, plugins)

  val imem = if (!imemHexPath.isEmpty) {
    val imem = Mem(UInt(config.xlen bits), new File(imemHexPath).length() / 4)
    HexTools.initRam(imem, imemHexPath, 0)
    imem
  } else {
    Mem(UInt(config.xlen bits), 1024)
  }

  val ibus = pipeline.getService[IBusService].getIBus
  ibus.rdata := imem(ibus.address.resized)

  var i: BigInt = 0
  val dmem = Mem(UInt(config.xlen bits), 1024)

  val dbus = pipeline.getService[DBusService].getDBus
  dbus.rdata := dmem(dbus.address.resized)
  dmem.write(dbus.address.resized, dbus.wdata, dbus.write, dbus.wmask)
}

object Core {
  def main(args: Array[String]) {
    SpinalVerilog(new Core(args(0)))
  }
}

object CoreSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new Core(args(0))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      for (i <- 0 to 100) {
        dut.clockDomain.waitSampling()
      }
    }
  }
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new Core("", true))
  }
}
