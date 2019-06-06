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
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreFormal)
  }
}

class CoreTest(memHexPath: String) extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createPipeline()

  val testDev = new MmioDevice {
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

  val testOut = master(Flow(UInt(config.xlen bits)))
  testOut << testDev.io

  val soc = new Soc(
    pipeline,
    Seq(
      MemSegment(0, 8192).init(memHexPath),
      MmioSegment(8192, testDev)
    )
  )
}

object CoreTestSim {
  def main(args: Array[String]) {
    var mainResult = 0

    SimConfig.withWave.compile(new CoreTest(args(0))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      var done = false

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.testOut.valid.toBoolean) {
          val result = dut.testOut.payload.toBigInt

          if (result == 0) {
            println("All tests passed")
          } else {
            println(s"Test $result failed")
            mainResult = 1
          }

          done = true
        }
      }
    }

    sys.exit(mainResult)
  }
}
