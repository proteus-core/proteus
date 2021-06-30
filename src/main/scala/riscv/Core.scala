package riscv

import riscv.plugins._
import riscv.soc._
import riscv.soc.devices._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.HexTools
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.uart._

object createStaticPipeline {
  def apply(disablePipelining: Boolean = false,
            extraPlugins: Seq[Plugin[Pipeline]] = Seq(),
            build: Boolean = true)
           (implicit conf: Config): StaticPipeline = {
    import riscv.plugins.scheduling.static._

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

    if (disablePipelining) {
      pipeline.addPlugin(new NoPipeliningScheduler)
    } else {
      pipeline.addPlugins(Seq(
        new Scheduler,
        new DataHazardResolver(firstRsReadStage = pipeline.execute)
      ))
    }

    pipeline.addPlugins(Seq(
      new MemoryBackbone,
      new Fetcher(pipeline.fetch),
      new Decoder(pipeline.decode),
      new RegisterFile(pipeline.decode, pipeline.writeback),
      new IntAlu(pipeline.execute),
      new Shifter(pipeline.execute),
      new Lsu(pipeline.memory),
      new BranchUnit(pipeline.execute),
      new PcManager(0x80000000L),
      new CsrFile(pipeline.writeback),
      new Timers,
      new MachineMode(pipeline.execute),
      new TrapHandler(pipeline.writeback),
      new Interrupts(pipeline.writeback),
      new MulDiv(pipeline.execute)
    ) ++ extraPlugins)

    if (build) {
      pipeline.build()
    }

    pipeline
  }
}

class Core(imemHexPath: String, formal: Boolean = false) extends Component {
  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createStaticPipeline()

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
      MemSegment(0x80000000L, 10 MiB).init(imemHexPath),
      MmioSegment(0x02000000L, new MachineTimers(pipeline)),
      MmioSegment(0x10000000L, charDev),
      MmioSegment(0x20000000L, byteDev)
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

      val byteDevSim = new sim.StdioByteDev(dut.byteIo)

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

class CoreFormal extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createStaticPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreFormal)
  }
}

class CoreTest(memHexPath: String) extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createStaticPipeline()

  val testDev = new TestDev
  val testOut = master(Flow(UInt(config.xlen bits)))
  testOut << testDev.io

  val dummyTimerIo = pipeline.getService[InterruptService].getMachineTimerIrqIo
  dummyTimerIo.update := False
  dummyTimerIo.interruptPending.assignDontCare()

  val external_irq = pipeline.getService[InterruptService].getExternalIrqIo
  external_irq.update := False
  external_irq.interruptPending := False

  val soc = new Soc(
    pipeline,
    Seq(
      MemSegment(0x80000000L, 10 MiB).init(memHexPath),
      MmioSegment(0x30000000L, testDev)
    )
  )
}

object CoreTestSim {
  def main(args: Array[String]) {
    var mainResult = 0

    SimConfig.withWave.compile(new CoreAxi4(RamType.OnChipRam(10 MiB, Some(args(0))))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      var done = false

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.io.testDev.valid.toBoolean) {
          val result = dut.io.testDev.payload.toBigInt

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

class CoreExtMem extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createStaticPipeline()

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
    SpinalVerilog(new CoreAxi4(RamType.ExternalAxi4(10 MiB)))
  }
}

sealed abstract class RamType(val size: BigInt)

object RamType {
  case class OnChipRam(override val size: BigInt, initHexFile: Option[String]) extends RamType(size)
  case class ExternalAxi4(override val size: BigInt) extends RamType(size)

}

class CoreAxi4(ramType: RamType) extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)

  val io = new Bundle{
    // Peripherals
    val charOut = master(Flow(UInt(8 bits)))
    val testDev = master(Flow(UInt(config.xlen bits)))
    val byteDev = master(new ByteDevIo)

    val axi = ramType match {
      case RamType.ExternalAxi4(size) =>
        val axiConfig = Axi4SharedOnChipRam.getAxiConfig(
          dataWidth = config.xlen,
          byteCount = size,
          idWidth = 4
        )

        master(Axi4Shared(axiConfig))
      case _ => null
    }
  }

  val socClockDomain = ClockDomain(
    clock = clockDomain.clock,
    reset = clockDomain.reset,
    frequency = FixedFrequency(100 MHz)
  )

  val coreClockDomain = ClockDomain(
    clock = clockDomain.clock,
    reset = clockDomain.reset
  )

  val soc = new ClockingArea(socClockDomain) {
    val core = new ClockingArea(coreClockDomain) {
      val pipeline = createStaticPipeline()

      val memService = pipeline.getService[MemoryService]
      val ibus = memService.getExternalIBus
      val dbus = memService.getExternalDBus
    }

    core.setName("")

    val ramAxi = ramType match {
      case RamType.ExternalAxi4(_) => io.axi
      case RamType.OnChipRam(size, initHexFile) =>
        val ram = Axi4SharedOnChipRam(
          byteCount = size,
          dataWidth = config.xlen,
          idWidth = 4
        )

        initHexFile.foreach(HexTools.initRam(ram.ram, _, 0x80000000L))
        ram.io.axi
    }

    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = config.dbusConfig.addressWidth,
      dataWidth = config.dbusConfig.dataWidth,
      idWidth = 4
    )

    val axiCrossbar = Axi4CrossbarFactory()

    // Without low latency, only one command every 2 cycles is accepted on the master bus which
    // wouldn't allow us to reach IPC=1. This could be fixed by using bursts on the ibus.
    axiCrossbar.lowLatency = true

    axiCrossbar.addSlaves(
      ramAxi           -> (0x80000000L, ramType.size),
      apbBridge.io.axi -> (0x00000000L, 1 GiB)
    )

    val ibusAxi = core.ibus.toAxi4ReadOnly()

    axiCrossbar.addConnections(
      ibusAxi -> List(ramAxi),
      core.dbus.toAxi4Shared()   -> List(ramAxi, apbBridge.io.axi)
    )

    // This pipelining is used to cut combinatorial loops caused by lowLatency=true. It is based on
    // advice from the Spinal developers: "m2sPipe is a full bandwidth master to slave cut,
    // s2mPipe is a full bandwidth slave to master cut".
    // TODO I should really read-up on this pipelining stuff...
    axiCrossbar.addPipelining(ibusAxi)((ibus, crossbar) => {
      ibus.readCmd.m2sPipe() >> crossbar.readCmd
      ibus.readRsp << crossbar.readRsp.s2mPipe()
    })

    axiCrossbar.build()

    val machineTimers = new Apb3MachineTimers(core.pipeline)

    val charDev = new Apb3CharDev
    io.charOut << charDev.io.char

    val testDev = new Apb3TestDev
    io.testDev << testDev.io.test

    val byteDev = new Apb3ByteDev
    io.byteDev <> byteDev.io.bytes
    core.pipeline.getService[InterruptService].getExternalIrqIo <> byteDev.io.irq

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        machineTimers.io.apb        -> (0x02000000L, 4 KiB),
        charDev.io.apb              -> (0x10000000L, 4 KiB),
        byteDev.io.apb              -> (0x20000000L, 4 KiB),
        testDev.io.apb              -> (0x30000000L, 4 KiB)
      )
    )
  }
}

object CoreAxi4 {
  def main(args: Array[String]) {
    var imemHexPath: String = null
    if (args.length > 0) {
      imemHexPath = args(0)
    }

    SpinalVerilog(new CoreAxi4(RamType.OnChipRam(10 MiB, Option(imemHexPath))))
  }
}

object CoreAxi4Sim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new CoreAxi4(RamType.OnChipRam(10 MiB, Some(args(0))))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      val byteDevSim = new sim.StdioByteDev(dut.io.byteDev)

      var done = false

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.io.charOut.valid.toBoolean) {
          val char = dut.io.charOut.payload.toInt.toChar

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

object createDynamicPipeline {
  def apply(extraPlugins: Seq[Plugin[Pipeline]] = Seq(), build: Boolean = true)
           (implicit conf: Config): DynamicPipeline = {
    val pipeline = new Component with DynamicPipeline {
      setDefinitionName("Pipeline")

      override val config = conf
      override val data = new StandardPipelineData(conf)
      override val pipelineComponent = this

      val dynamicPipeline: DynamicPipeline = this

      override val issuePipeline = new StaticPipeline {
        val fetch = new Stage("IF").setName("fetch")
        val decode = new Stage("ID").setName("decode")

        override val stages = Seq(fetch, decode)
        override val config = dynamicPipeline.config
        override val data = dynamicPipeline.data
        override val pipelineComponent = dynamicPipeline.pipelineComponent
      }

      val intAlu = new Stage("EX_ALU")
      val intMul = new Stage("EX_MUL")
      override val exeStages: Seq[Stage] = Seq(intAlu, intMul)
    }

    pipeline.issuePipeline.addPlugins(Seq(
      new scheduling.static.Scheduler(canStallExternally = true),
      new scheduling.static.PcManager(0x80000000L),
      new MemoryBackbone,
      new Fetcher(pipeline.issuePipeline.fetch),
      new Decoder(pipeline.issuePipeline.decode)
    ))

    pipeline.addPlugins(Seq(
      new scheduling.dynamic.Scheduler,
      new IntAlu(pipeline.intAlu),
      new MulDiv(pipeline.intMul)
    ))

    if (build) {
      pipeline.build()
    }

    pipeline
  }
}

class CoreDynamic(imemHexPath: String, formal: Boolean = false) extends Component {
  setDefinitionName("Core")
  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createDynamicPipeline()

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io

  val soc = new Soc(
    pipeline,
    Seq(
      MemSegment(0x80000000L, 10 MiB).init(imemHexPath),
      MmioSegment(0x10000000L, charDev)
    )
  )
}

object CoreDynamic {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreDynamic(args(0)))
  }
}

object CoreDynamicSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new CoreDynamic(args(0))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      var done = false
      var i = 0

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

        i += 1

        if (i == 100) {
          done = true
        }
      }
    }
  }
}
