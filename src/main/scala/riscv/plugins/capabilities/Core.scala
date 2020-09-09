package riscv.plugins.capabilities

import riscv._
import riscv.soc._
import riscv.soc.devices._
import riscv.sim._

import spinal.core._
import spinal.lib._
import spinal.core.sim._

object createCheriPipeline {
  def apply()(implicit config: Config): Pipeline = {
    val pipeline = createStaticPipeline(build = false)

    implicit val context = Context(pipeline)
    pipeline.addPlugins(Seq(
      new RegisterFile(pipeline.stages(1), pipeline.stages.last),
      new Access(pipeline.stages(2)),
      new ScrFile(pipeline.stages.last),
      new Lsu(pipeline.stages(3)),
      new ExceptionHandler,
      new Ccsr,
      new MemoryTagger(0x0, 10 MiB)
    ))

    pipeline
  }
}

class Core(imemHexPath: String) extends Component {
  implicit val config = new Config(BaseIsa.RV32I)

  val pipeline = createCheriPipeline()
  pipeline.build()

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io

  val byteDev = new ByteDev
  val byteIo = master(new ByteDevIo)
  byteIo <> byteDev.io
  byteDev.irq <> pipeline.getService[InterruptService].getExternalIrqIo

  val soc = new Soc(
    pipeline,
    Seq(
      MemSegment(0x0, 10 MiB).init(imemHexPath),
      MmioSegment(0xf0001000L, new MachineTimers(pipeline)),
      MmioSegment(0xf0002000L, charDev),
      MmioSegment(0xf0004000L, byteDev)
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

      val byteDevSim = new StdioByteDev(dut.byteIo)

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

        byteDevSim.eval()
      }
    }
  }
}

class CoreExtMem extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createCheriPipeline()
  pipeline.build()

  val ibus = master(new MemBus(config.ibusConfig))
  val dbus = master(new MemBus(config.dbusConfig))

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io

  val testDev = new TestDev
  val testOut = master(Flow(UInt(config.xlen bits)))
  testOut << testDev.io

  val byteDev = new ByteDev
  val byteIo = master(new ByteDevIo)
  byteIo <> byteDev.io
  byteDev.irq <> pipeline.getService[InterruptService].getExternalIrqIo

  val soc = new Soc(
    pipeline,
    Seq(
      MemBusSegment(0x0, 10 MiB, dbus, ibus),
      MmioSegment(0xf0001000L, new MachineTimers(pipeline)),
      MmioSegment(0xf0002000L, charDev),
      MmioSegment(0xf0003000L, testDev),
      MmioSegment(0xf0004000L, byteDev)
    )
  )
}

object CoreExtMem {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreExtMem)
  }
}
