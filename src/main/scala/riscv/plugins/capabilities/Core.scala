package riscv.plugins.capabilities

import riscv._
import riscv.soc._
import riscv.soc.devices._
import riscv.sim._

import spinal.core._
import spinal.lib._
import spinal.core.sim._

object createCheriPipeline {
  def apply()(implicit conf: Config): StaticPipeline = {
    val pipeline = new Component with StaticPipeline {
      setDefinitionName("Pipeline")

      val fetch = new Stage("IF")
      val decode = new Stage("ID")
      val execute = new Stage("EX")
      val memory = new Stage("MEM")
      val writeback = new Stage("WB")

      override val stages = Seq(fetch, decode, execute, memory, writeback)
      override val config: Config = conf
      override val data: StandardPipelineData = new StandardPipelineData(conf)
      override val pipelineComponent: Component = this
    }

    import riscv.{plugins => rvp}

    pipeline.addPlugins(Seq(
      new rvp.scheduling.static.Scheduler,
      new rvp.scheduling.static.DataHazardResolver(firstRsReadStage = pipeline.execute),
      new rvp.scheduling.static.TrapHandler(pipeline.writeback),
      new rvp.MemoryBackbone,
      new rvp.Fetcher(pipeline.fetch),
      new rvp.Decoder(pipeline.decode),
      new rvp.RegisterFile(pipeline.decode, pipeline.writeback),
      new rvp.IntAlu(pipeline.execute),
      new rvp.Shifter(pipeline.execute),
      new rvp.Lsu(pipeline.memory),
      new rvp.BranchUnit(pipeline.execute),
      new rvp.scheduling.static.PcManager(0x80000000L),
      new rvp.CsrFile(pipeline.writeback),
      new rvp.Timers,
      new rvp.MachineMode(pipeline.execute, addMepc = false, addMtvec = false),
      new rvp.Interrupts(pipeline.writeback),
      new rvp.MulDiv(pipeline.execute)
    ))

    implicit val context = Context(pipeline)

    pipeline.addPlugins(Seq(
      new RegisterFile(pipeline.decode, pipeline.writeback),
      new Access(pipeline.execute),
      new ScrFile(pipeline.writeback),
      new Lsu(pipeline.memory),
      new ExceptionHandler,
      new Ccsr,
      new MemoryTagger(0x0, 10 MiB),
      new PccManager(pipeline.execute),
      new Sealing(pipeline.execute)
    ))

    pipeline.build()
    pipeline
  }
}

class Core(imemHexPath: String) extends Component {
  implicit val config = new Config(BaseIsa.RV32I)

  val pipeline = createCheriPipeline()

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
      MmioSegment(0x02000000L, new MachineTimers(pipeline)),
      MmioSegment(0x10000000L, charDev),
      MmioSegment(0x20000000L, byteDev),
      MemSegment( 0x80000000L, 10 MiB).init(imemHexPath)
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
      MmioSegment(0x02000000L, new MachineTimers(pipeline)),
      MmioSegment(0x10000000L, charDev),
      MmioSegment(0x20000000L, byteDev),
      MmioSegment(0x30000000L, testDev),
      MemBusSegment(0x80000000L, 10 MiB, dbus, ibus)
    )
  )
}

object CoreExtMem {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreExtMem)
  }
}
