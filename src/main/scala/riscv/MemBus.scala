package riscv

import spinal.core._
import spinal.lib.IMasterSlave

class MemBus(width: Int) extends Bundle with IMasterSlave {
  val address = UInt(width bits)
  val read, write = Bool()
  val rdata, wdata = UInt(width bits)
  val wmask = Bits(width / 8 bits)

  def byte2WordAddress(ba: UInt): UInt = ba >> log2Up(width / 8)
  def word2ByteAddress(wa: UInt): UInt = wa << log2Up(width / 8)
  def byteAddress: UInt = word2ByteAddress(address)

  override def asMaster(): Unit = {
    in(address, read, write, wdata, wmask)
    out(rdata)
  }
}
