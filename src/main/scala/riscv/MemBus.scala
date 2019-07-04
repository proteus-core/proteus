package riscv

import spinal.core._
import spinal.lib._

case class MemBusConfig(
  width: Int,
  readWrite: Boolean = true
)

case class MemBusCmd(config: MemBusConfig) extends Bundle {
  val address = UInt(config.width bits)
  val write = if (config.readWrite) Bool() else null
  val wdata = if (config.readWrite) UInt(config.width bits) else null
  val wmask = if (config.readWrite) Bits(config.width / 8 bits) else null
}

case class MemBusRsp(config: MemBusConfig) extends Bundle {
  val rdata = UInt(config.width bits)
}

class MemBus(val config: MemBusConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(MemBusCmd(config))
  val rsp = Stream(MemBusRsp(config))

  def byte2WordAddress(ba: UInt): UInt = ba >> log2Up(config.width / 8)
  def word2ByteAddress(wa: UInt): UInt = wa << log2Up(config.width / 8)

  override def asMaster(): Unit = {
    slave(cmd)
    master(rsp)
  }
}
