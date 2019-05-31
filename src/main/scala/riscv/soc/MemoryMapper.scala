package riscv.soc

import riscv._

import spinal.core._
import spinal.lib._

sealed trait MemorySegment {
  def start: Int
  def length: Int
  def bus: MemBus
  def end: Int = start + length
}

case class MemBusSegment(start: Int, length: Int, bus: MemBus) extends MemorySegment

case class MmioSegment(start: Int, device: MmioDevice) extends MemorySegment {
  val length = device.size
  val bus = device.bus
}

class MemoryMapper(segments: Seq[MemorySegment])(implicit config: Config) extends Component {
  val bus = master(new MemBus(config.xlen))
  bus.rdata.assignDontCare()

  val slaves = segments.map {segment =>
    val bus = slave(new MemBus(config.xlen))
    bus.address.assignDontCare()
    bus.read := False
    bus.write := False
    bus.wdata.assignDontCare()
    bus.wmask.assignDontCare()

    parent.rework {
      segment.bus <> bus
    }

    bus
  }

  for ((segment, slave) <- segments.zip(slaves)) {
    // Verilator errs when checking if a signal is >= 0 because this comparison
    // is always true.
    val lowerBoundCheck = if (segment.start == 0) {
      True
    } else {
      bus.byteAddress >= segment.start
    }

    when (lowerBoundCheck && bus.byteAddress < segment.end) {
      bus <> slave
      slave.address.allowOverride
      slave.address := bus.address - bus.byte2WordAddress(segment.start)
    }
  }
}
