package riscv

import riscv.plugins._
import riscv.soc._
import riscv.soc.devices._

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object createPipeline {
  def apply(disablePipelining: Boolean = false,
            extraPlugins: Seq[Plugin] = Seq())
           (implicit config: Config): Pipeline = {
    val pipelinePlugins = if (disablePipelining) {
      Seq(new NoPipelining)
    } else {
      Seq(new SimplePipelining, new DataHazardResolver)
    }

    val plugins = pipelinePlugins ++ Seq(
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

    new Pipeline(config, plugins)
  }
}

class Core(imemHexPath: String, formal: Boolean = false) extends Component {
  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createPipeline()

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io

  val soc = new Soc(
    pipeline,
    Seq(
      MemSegment(0, 2048).init(imemHexPath),
      MmioSegment(2048, new MachineTimers()),
      MmioSegment(2064, charDev)
    )
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

      var done = false

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.charOut.valid.toBoolean) {
          val char = dut.charOut.payload.toInt.toChar

          if (char == 4) {
            println("Simulation halted by software")
            done = true
          } else {
            print(char)
          }
        }
      }
    }
  }
}

class CoreFormal extends Component {
  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreFormal)
  }
}
