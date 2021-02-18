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
    SpinalVerilog(new CoreExtMem)
  }
}

class CoreAxi4(imemHexPath: Option[String]) extends Component {
  setDefinitionName("Core")

  val io = new Bundle{
    // Peripherals
    val uart = master(Uart())
  }

  implicit val config = new Config(BaseIsa.RV32I)

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

    val ram = Axi4SharedOnChipRam(
      byteCount = 4 KiB,
      dataWidth = config.xlen,
      idWidth = 4
    )
    if (!imemHexPath.isEmpty) {
      HexTools.initRam(ram.ram, imemHexPath.get, 0x00000000L)
    }

    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = config.dbusConfig.addressWidth - 8,
      dataWidth = config.dbusConfig.dataWidth,
      idWidth = 4
    )

    val axiCrossbar = Axi4CrossbarFactory()
    axiCrossbar.addSlaves(
      ram.io.axi       -> (0x00000000L, 4 KiB),
      apbBridge.io.axi -> (0xF0000000L, 1 MiB)
    )
    axiCrossbar.addConnections(
      core.ibus.toAxi4ReadOnly() -> List(ram.io.axi),
      core.dbus.toAxi4Shared()   -> List(ram.io.axi, apbBridge.io.axi)
    )
    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar, bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    axiCrossbar.build()

    val uartCtrlConfig = UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(),
      initConfig = UartCtrlInitConfig(
        baudrate = 115200,
        dataLength = 7,
        parity = UartParityType.NONE,
        stop = UartStopType.ONE
      )
    )
    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)

    val machineTimers = new MachineTimers(core.pipeline)

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        uartCtrl.io.apb             -> (0x000000L, 4 KiB),
        machineTimers.dbus.toApb3() -> (0x001000L, 4 KiB)
      )
    )
  }

  io.uart <> soc.uartCtrl.io.uart
}

object CoreAxi4 {
  def main(args: Array[String]) {
    var imemHexPath: String = null
    if (args.length > 0) {
      imemHexPath = args(0)
    }

    SpinalVerilog(new CoreAxi4(Option(imemHexPath)))
  }
}

object CoreAxi4Sim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new CoreAxi4(Some(args(0)))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      var done = false
      var cycles = 0

      dut.clockDomain.onRisingEdges {
        cycles += 1
      }

      while (!done) {
        dut.clockDomain.waitSampling()

        if (cycles >= 10000) {
          done = true
        }
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
      new scheduling.static.PcManager,
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
      MemSegment(0x0, 1 MiB).init(imemHexPath),
      MmioSegment(0xf0002000L, charDev)
    )
  )
}

object CoreDynamic {
  def main(args: Array[String]) {
    SpinalVerilog(new Core(args(0)))
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
