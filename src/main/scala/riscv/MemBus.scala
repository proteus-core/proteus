package riscv

import spinal.core._
import spinal.lib.IMasterSlave

class MemBus(width: Int) extends Bundle with IMasterSlave {
  val address = UInt(width bits)
  val write = Bool()
  val rdata, wdata = UInt(width bits)
  val wmask = Bits(width / 8 bits)

  override def asMaster(): Unit = {
    in(address, write, wdata, wmask)
    out(rdata)
  }
}
