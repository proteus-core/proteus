package riscv.plugins.cheri

import spinal.core._
import spinal.lib._

case class CapBusCmd(implicit context: Context) extends Bundle {
  val address = UInt(context.config.xlen bits)
  val write = Bool()
  val wdata = MemCapability()
}

case class CapBusRsp(implicit context: Context) extends Bundle {
  val rdata = MemCapability()
}

case class CapBus(implicit context: Context) extends Bundle with IMasterSlave {
  val cmd = Stream(CapBusCmd())
  val rsp = Stream(CapBusRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}
