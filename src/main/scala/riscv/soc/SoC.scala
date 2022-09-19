package riscv.soc

import riscv._
import riscv.soc.devices._

import spinal.core._
import spinal.lib._
import spinal.lib.misc.HexTools
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba3.apb._

sealed abstract class RamType(val size: BigInt)

object RamType {
  case class OnChipRam(override val size: BigInt, initHexFile: Option[String]) extends RamType(size)
  case class ExternalAxi4(override val size: BigInt) extends RamType(size)
}

class SoC(
    ramType: RamType,
    createPipeline: Config => Pipeline,
    extraDbusReadDelay: Int = 0
) extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)

  val io = new Bundle {
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
      val pipeline = createPipeline(config)

      val memService = pipeline.service[MemoryService]
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
      ramAxi -> (0x80000000L, ramType.size),
      apbBridge.io.axi -> (0x00000000L, 1 GiB)
    )

    val ibusAxi = core.ibus.toAxi4ReadOnly()
    val dbusAxi = core.dbus.toAxi4Shared()

    axiCrossbar.addConnections(
      ibusAxi -> List(ramAxi),
      dbusAxi -> List(ramAxi, apbBridge.io.axi)
    )

    // This pipelining is used to cut combinatorial loops caused by lowLatency=true. It is based on
    // advice from the Spinal developers: "m2sPipe is a full bandwidth master to slave cut,
    // s2mPipe is a full bandwidth slave to master cut".
    // TODO I should really read-up on this pipelining stuff...
    axiCrossbar.addPipelining(ibusAxi)((ibus, crossbar) => {
      ibus.readCmd.m2sPipe() >> crossbar.readCmd
      ibus.readRsp << crossbar.readRsp.s2mPipe()
    })

    if (extraDbusReadDelay > 0) {
      axiCrossbar.addPipelining(dbusAxi)((dbus, crossbar) => {
        import Utils._

        dbus.sharedCmd >> crossbar.sharedCmd
        dbus.writeData >> crossbar.writeData
        dbus.readRsp << crossbar.readRsp.stage(extraDbusReadDelay)
        dbus.writeRsp << crossbar.writeRsp
      })
    }

    axiCrossbar.build()

    val machineTimers = new Apb3MachineTimers(core.pipeline)

    val charDev = new Apb3CharDev
    io.charOut << charDev.io.char

    val testDev = new Apb3TestDev
    io.testDev << testDev.io.test

    val byteDev = new Apb3ByteDev
    io.byteDev <> byteDev.io.bytes

    core.pipeline.serviceOption[InterruptService] foreach { interruptService =>
      interruptService.getExternalIrqIo <> byteDev.io.irq
    }

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        machineTimers.io.apb -> (0x02000000L, 4 KiB),
        charDev.io.apb -> (0x10000000L, 4 KiB),
        byteDev.io.apb -> (0x20000000L, 4 KiB),
        testDev.io.apb -> (0x30000000L, 4 KiB)
      )
    )
  }
}
