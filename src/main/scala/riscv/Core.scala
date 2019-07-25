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
      new TrapHandler,
      new Interrupts,
      new MulDiv
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
      MemSegment(0, 102400).init(imemHexPath),
      MmioSegment(102400, new MachineTimers(pipeline)),
      MmioSegment(102416, charDev)
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

  val testDev = new TestDev
  val testOut = master(Flow(UInt(config.xlen bits)))
  testOut << testDev.io

  val dummyTimerIo = pipeline.getService[InterruptService].getMachineTimerIo
  dummyTimerIo.update := False
  dummyTimerIo.interruptPending.assignDontCare()

  val soc = new Soc(
    pipeline,
    Seq(
      MemSegment(0, 8192).init(memHexPath),
      MmioSegment(102420, testDev)
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
  val pipeline = createPipeline()

  val ibus = master(new MemBus(config.ibusConfig))
  val dbus = master(new MemBus(config.dbusConfig))

  val charDev = new CharDev
  val charOut = master(Flow(UInt(8 bits)))
  charOut << charDev.io

  val testDev = new TestDev
  val testOut = master(Flow(UInt(config.xlen bits)))
  testOut << testDev.io

  val soc = new Soc(
    pipeline,
    Seq(
      MemBusSegment(0, 102400, dbus, ibus),
      MmioSegment(102400, new MachineTimers(pipeline)),
      MmioSegment(102416, charDev),
      MmioSegment(102420, testDev)
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
      val pipeline = createPipeline()

      val ibus = pipeline.getService[IBusService].getIBus
      val dbus = pipeline.getService[DBusService].getDBus
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
