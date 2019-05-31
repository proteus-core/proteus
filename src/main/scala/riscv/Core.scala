package riscv

import riscv.plugins._
import riscv.soc._
import riscv.soc.devices._
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

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io

  val soc = new Soc(
    pipeline,
    Seq(
      MmioSegment(0, new MachineTimers()),
      MmioSegment(16, charDev),
      MemSegment(1024, 1024)
    ),
    imemHexPath
  )
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
