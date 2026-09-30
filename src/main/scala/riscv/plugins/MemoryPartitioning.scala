package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class MemoryPartitioning(boundaryCsrIds: Set[(Int, Int)] = Set((0x707, 0x708), (0x709, 0x70a)))(
    implicit config: Config
) extends Plugin[Pipeline]
    with MemoryPartitioningService {
  private var boundaryCsrs: Set[(CsrIo, CsrIo)] = null

  private class boundaryCsr(implicit config: Config) extends Csr {
    val addr = Reg(UInt(config.isa.xlen bits)).init(0)
    override def read(): UInt = addr
    override def write(addr: UInt): Unit = this.addr := addr
  }

  private def isInPartition(
      address: UInt,
      boundaries: Set[(CsrIo, CsrIo)],
      insignificantBits: Int
  ): Bool = {
    // ignore the lowest insignificant bits:
    // for address >= low: map corresponding low bits to zero
    // for address < high: map corresponding address bits to zero
    (boundaries foldLeft False) {
      case (acc, (low, high)) => {
        acc || (address >= (low.read() >> insignificantBits << insignificantBits) &&
          (address >> insignificantBits << insignificantBits) < high.read())
      }
    }
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      boundaryCsrIds map {
        case (low, high) => {
          csrService.registerCsr(low, new boundaryCsr)
          csrService.registerCsr(high, new boundaryCsr)
        }
      }
    }
  }

  override def isInPartition(isStage: Boolean, address: UInt, insignificantBits: Int = 0): Bool = {
    if (isStage) { // build new CsrIos
      def readOnlyCsr(csrId: Int): CsrIo = {
        val csrIn = slave(new CsrIo)
        pipeline plug new Area {
          csrIn <> pipeline.service[CsrService].getCsr(csrId)
        }
        csrIn.wdata.assignDontCare()
        csrIn.write := False
        csrIn
      }
      val newBoundaryCsrs = boundaryCsrIds map {
        case (low, high) => {
          (readOnlyCsr(low), readOnlyCsr(high))
        }
      }
      isInPartition(address, newBoundaryCsrs, insignificantBits)
    } else {
      if (boundaryCsrs == null) {
        pipeline plug new Area {
          def readOnlyCsr(csrId: Int): CsrIo = {
            val csr = pipeline.service[CsrService].getCsr(csrId)
            csr.wdata.assignDontCare()
            csr.write := False
            csr
          }
          boundaryCsrs = boundaryCsrIds map {
            case (low, high) => {
              (readOnlyCsr(low), readOnlyCsr(high))
            }
          }
        }
      }
      // use the boundaryCsrs plugged into `pipeline`
      isInPartition(address, boundaryCsrs, insignificantBits)
    }
  }
}
