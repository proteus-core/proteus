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

class MemBusControl(bus: MemBus)(implicit config: Config) extends Area {
  // To make sure we can properly interface with AXI4 busses, we have to ensure
  // that cmd.valid stays asserted until cmd.ready is asserted. Therefore, we
  // buffer one command so that if our client does not call read/write until the
  // command is completed, we can still adhere to the protocol.
  val currentCmd = new Bundle {
    val valid = Reg(Bool()).init(False)
    val ready = Reg(Bool()).init(False)
    val cmd = Reg(MemBusCmd(bus.config))

    def isIssued = valid || ready
    def isWrite = if (bus.config.readWrite) cmd.write else False
  }

  bus.cmd.valid := currentCmd.valid
  bus.cmd.address := currentCmd.cmd.address

  if (bus.config.readWrite) {
    bus.cmd.write := currentCmd.cmd.write
    bus.cmd.wdata := currentCmd.cmd.wdata
    bus.cmd.wmask := currentCmd.cmd.wmask
  }

  bus.rsp.ready := True

  when (bus.cmd.valid && bus.cmd.ready) {
    currentCmd.ready := True
    currentCmd.valid := False
  }

  when (bus.rsp.valid) {
    currentCmd.ready := False
    currentCmd.valid := False
  }

  private def issueCommand(address: UInt, write: Boolean = false,
                           wdata: UInt = null, wmask: Bits = null) = {
    assert(!write || bus.config.readWrite)

    currentCmd.valid := True
    currentCmd.cmd.address := address
    bus.cmd.valid := True
    bus.cmd.address := address

    if (bus.config.readWrite) {
      if (write) {
        currentCmd.cmd.write := True
        currentCmd.cmd.wdata := wdata
        currentCmd.cmd.wmask := wmask
        bus.cmd.write := True
        bus.cmd.wdata := wdata
        bus.cmd.wmask := wmask
      } else {
        currentCmd.cmd.write := False
        bus.cmd.write := False
      }
    }
  }

  def read(address: UInt): (Bool, UInt) = {
    val valid = False
    val rdata = U(0, bus.config.width bits)
    val dropRsp = False

    when (!currentCmd.isIssued) {
      issueCommand(address)
    } elsewhen (currentCmd.cmd.address =/= address) {
      dropRsp := True
    }

    when (bus.rsp.valid) {
      currentCmd.valid := False
      currentCmd.ready := False

      when (!dropRsp && !currentCmd.isWrite) {
        valid := True
        rdata := bus.rsp.rdata
      }
    }

    (valid, rdata)
  }

  def write(address: UInt, wdata: UInt, wmask: Bits): Bool = {
    assert(bus.config.readWrite)

    val accepted = False
    val dropRsp = False

    when (!currentCmd.isIssued) {
      issueCommand(address, write = true, wdata, wmask)
    } elsewhen (currentCmd.cmd.address =/= address) {
      dropRsp := True
    }

    when (bus.cmd.ready) {
      currentCmd.valid := False
      currentCmd.ready := False

      when (!dropRsp && currentCmd.isWrite) {
        accepted := True
      }
    }

    accepted
  }
}
