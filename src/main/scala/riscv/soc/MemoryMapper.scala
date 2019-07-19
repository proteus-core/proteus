package riscv.soc

import riscv._

import spinal.core._
import spinal.lib._
import spinal.lib.misc.HexTools

sealed trait MemorySegment {
  def start: Int
  def length: Int
  def dbus: MemBus
  def ibus: MemBus = null
  def end: Int = start + length
}

case class MemBusSegment(start: Int, length: Int, dbus: MemBus,
                         override val ibus: MemBus = null) extends MemorySegment

case class MemSegment(start: Int, length: Int,
                      ibusLatency: Int = 0,
                      dbusReadLatency: Int = 0,
                      dbusWriteLatency: Int = 0)
                     (implicit config: Config) extends MemorySegment {
  val mem = Mem(UInt(config.xlen bits), length / (config.xlen / 8))

  override val ibus = new MemBus(config.ibusConfig)
  ibus.cmd.ready := False
  ibus.rsp.valid := False
  ibus.rsp.rdata.assignDontCare()

  when (ibus.cmd.valid) {
    Utils.delay(ibusLatency) {
      ibus.cmd.ready := True
      ibus.rsp.valid := True
      ibus.rsp.rdata := mem(ibus.byte2WordAddress(ibus.cmd.address).resized)
    }
  }

  val dbus = new MemBus(config.dbusConfig)
  dbus.cmd.ready := False
  dbus.rsp.valid := False
  dbus.rsp.rdata.assignDontCare()

  when (dbus.cmd.valid) {
    when (dbus.cmd.write) {
      Utils.delay(dbusWriteLatency) {
        dbus.cmd.ready := True
        mem.write(dbus.byte2WordAddress(dbus.cmd.address).resized,
                  dbus.cmd.wdata, mask = dbus.cmd.wmask)
      }
    } otherwise {
      Utils.delay(dbusReadLatency) {
        dbus.cmd.ready := True
        dbus.rsp.valid := True
        dbus.rsp.rdata := mem(dbus.byte2WordAddress(dbus.cmd.address).resized)
      }
    }
  }

  def init(memHexPath: String): this.type = {
    HexTools.initRam(mem, memHexPath, start)
    this
  }
}

case class MmioSegment(start: Int, device: MmioDevice) extends MemorySegment {
  val length = device.size
  val dbus = device.dbus
}

class MemoryMapper(segments: Seq[MemorySegment])(implicit config: Config) extends Component {
  val ibus = slave(new MemBus(config.ibusConfig))
  ibus.cmd.ready := False
  ibus.rsp.valid := False
  ibus.rsp.rdata.assignDontCare()
  val dbus = slave(new MemBus(config.dbusConfig))
  dbus.cmd.ready := False
  dbus.rsp.valid := False
  dbus.rsp.rdata.assignDontCare()

  val slaves = segments.map {segment =>
    def createSlaveBus(segmentBus: MemBus) = {
      val bus = master(new MemBus(segmentBus.config))
      bus.cmd.valid := False
      bus.rsp.ready := False

      bus.cmd.address.assignDontCare()
      if (bus.config.readWrite) {
        bus.cmd.write := False
        bus.cmd.wdata.assignDontCare()
        bus.cmd.wmask.assignDontCare()
      }

      parent.rework {
        segmentBus <> bus
      }

      bus
    }

    val islave = if (segment.ibus == null) {
      null
    } else {
      createSlaveBus(segment.ibus).setName("islave")
    }

    val dslave = createSlaveBus(segment.dbus).setName("dslave")

    (islave, dslave)
  }

  for ((segment, (islave, dslave)) <- segments.zip(slaves)) {
    def connectSlave(master: MemBus, slave: MemBus): Unit = {
      // Verilator errs when checking if a signal is >= 0 because this
      // comparison is always true.
      val lowerBoundCheck = if (segment.start == 0) {
        True
      } else {
        master.cmd.address >= segment.start
      }

      when (lowerBoundCheck && master.cmd.address < segment.end) {
        val address = master.cmd.address - segment.start

        slave.cmd.arbitrationFrom(master.cmd)
        master.rsp.arbitrationFrom(slave.rsp)

        slave.cmd.address := address.resized
        if (master.config.readWrite) {
          slave.cmd.write := master.cmd.write
          slave.cmd.wdata := master.cmd.wdata
          slave.cmd.wmask := master.cmd.wmask
        }

        master.rsp.rdata := slave.rsp.rdata
      }
    }

    if (islave != null) {
      connectSlave(ibus, islave)
    }

    connectSlave(dbus, dslave)
  }
}
