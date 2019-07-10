package riscv

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly, Axi4Shared}

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

  def toAxi4ReadOnly(): Axi4ReadOnly = {
    val axi4Config = MemBus.getAxi4Config(config.width)
    val axi4Bus = Axi4ReadOnly(axi4Config)

    axi4Bus.readCmd.valid := cmd.valid
    axi4Bus.readCmd.addr := cmd.address
    cmd.ready := axi4Bus.readCmd.ready

    rsp.valid := axi4Bus.readRsp.valid
    rsp.rdata := axi4Bus.readRsp.data.asUInt
    axi4Bus.readRsp.ready := rsp.ready

    axi4Bus
  }

  def toAxi4Shared(): Axi4Shared = {
    val axi4Config = MemBus.getAxi4Config(config.width)
    val axi4Bus = Axi4Shared(axi4Config)

    axi4Bus.sharedCmd.valid := cmd.valid
    axi4Bus.sharedCmd.addr := cmd.address
    axi4Bus.sharedCmd.write := cmd.write
    cmd.ready := axi4Bus.sharedCmd.ready

    axi4Bus.writeData.valid := cmd.valid
    axi4Bus.writeData.data := cmd.wdata.asBits
    axi4Bus.writeData.strb := cmd.wmask
    axi4Bus.writeData.last := True

    // TODO: Set useResp to true and verify response?
    axi4Bus.writeRsp.ready := rsp.ready
    axi4Bus.readRsp.ready := rsp.ready
    rsp.valid := axi4Bus.writeRsp.valid || axi4Bus.readRsp.valid
    rsp.rdata := axi4Bus.readRsp.data.asUInt

    axi4Bus
  }
}

object MemBus {
  def getAxi4Config(width: Int) = Axi4Config(
    addressWidth = width,
    dataWidth = width,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useCache = false,
    useSize = false,
    useQos = false,
    useLen = false,
    useLast = true,
    useResp = false,
    useProt = false,
    useStrb = true
  )
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

    when (bus.cmd.ready) {
      currentCmd.valid := False
      currentCmd.ready := True
    } otherwise {
      currentCmd.valid := True
      currentCmd.ready := False
    }

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
