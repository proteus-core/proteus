package riscv

import riscv.plugins._
import java.io.File

import spinal.core._
import spinal.core.sim._
import spinal.lib._
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
    new Shifter,
    new Lsu,
    new BranchUnit,
    new JumpResolver,
    new CsrFile,
    new Timers,
    new MachineMode,
    new TrapHandler
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

  val dmem = Mem(UInt(config.xlen bits), 1024)
  val dmemBus = new MemBus(config.xlen)
  val dmemAddr = dmemBus.address
  dmemBus.rdata := dmem(dmemAddr.resized)
  dmem.write(dmemAddr.resized, dmemBus.wdata, dmemBus.write, dmemBus.wmask)
  val dmemSegment = MemBusSegment(1024, dmem.width * dmem.wordCount, dmemBus)

  val mtimers = new MachineTimers
  val mtimersSegment = MmioSegment(0, mtimers)

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io
  val charDevSegment = MmioSegment(16, charDev)

  val memMapper = new MemoryMapper(Seq(mtimersSegment, charDevSegment, dmemSegment))

  val dbus = pipeline.getService[DBusService].getDBus
  dbus <> memMapper.bus
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

        if (dut.charOut.valid.toBoolean) {
          print(dut.charOut.payload.toInt.toChar)
        }
      }
    }
  }
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new Core("", true))
  }
}
