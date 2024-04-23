package riscv

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._

case class MemBusConfig(
    addressWidth: Int,
    idWidth: Int,
    dataWidth: Int,
    readWrite: Boolean = true
) {
  def byte2WordAddress(ba: UInt): UInt = ba(dataWidth - 1 downto log2Up(dataWidth / 8))
  def word2ByteAddress(wa: UInt): UInt = wa << log2Up(dataWidth / 8)
}

case class MemBusCmd(config: MemBusConfig) extends Bundle {
  val address = UInt(config.addressWidth bits)
  val id = UInt(config.idWidth bits)
  val write = if (config.readWrite) Bool() else null
  val wdata = if (config.readWrite) UInt(config.dataWidth bits) else null
  val wmask = if (config.readWrite) Bits(config.dataWidth / 8 bits) else null
}

case class MemBusRsp(config: MemBusConfig) extends Bundle {
  val rdata = UInt(config.dataWidth bits)
  val id = UInt(config.idWidth bits)
}

case class MemBus(val config: MemBusConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(MemBusCmd(config))
  val rsp = Stream(MemBusRsp(config))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  // TODO: id implementation impossible with Apb3!
  def toApb3(): Apb3 = {
    assert(!isMasterInterface)

    var apb3Config = MemBus.getApb3Config(config)
    val apb3Bus = Apb3(apb3Config)

    cmd.valid := apb3Bus.PSEL.asBool
    cmd.address := apb3Bus.PADDR
    cmd.write := apb3Bus.PWRITE
    cmd.wdata := apb3Bus.PWDATA.asUInt

    apb3Bus.PREADY := rsp.valid
    apb3Bus.PRDATA := rsp.rdata.asBits
    rsp.ready := apb3Bus.PENABLE

    apb3Bus
  }

  def toAxi4ReadOnly(): Axi4ReadOnly = {
    assert(isMasterInterface)

    val axi4Config = MemBus.getAxi4Config(config)
    val axi4Bus = Axi4ReadOnly(axi4Config)

    axi4Bus.readCmd.valid := cmd.valid
    axi4Bus.readCmd.addr := cmd.address
    axi4Bus.readCmd.id := cmd.id
    cmd.ready := axi4Bus.readCmd.ready

    rsp.valid := axi4Bus.readRsp.valid
    rsp.id := axi4Bus.readRsp.id
    rsp.rdata := axi4Bus.readRsp.data.asUInt
    axi4Bus.readRsp.ready := rsp.ready

    axi4Bus
  }

  def toAxi4Shared(): Axi4Shared = {
    assert(isMasterInterface)

    val axi4Config = MemBus.getAxi4Config(config)
    val axi4Bus = Axi4Shared(axi4Config)

    axi4Bus.sharedCmd.valid := cmd.valid
    axi4Bus.sharedCmd.addr := cmd.address
    axi4Bus.sharedCmd.write := cmd.write
    axi4Bus.sharedCmd.id := cmd.id
    cmd.ready := axi4Bus.sharedCmd.ready

    axi4Bus.writeData.valid := cmd.write && cmd.valid
    axi4Bus.writeData.data := cmd.wdata.asBits
    axi4Bus.writeData.strb := cmd.wmask
    axi4Bus.writeData.last := True

    // TODO: Set useResp to true and verify response?
    axi4Bus.writeRsp.ready := True // FIXME is this ok?
    axi4Bus.readRsp.ready := rsp.ready
    rsp.valid := axi4Bus.readRsp.valid
    rsp.id := axi4Bus.readRsp.id
    rsp.rdata := axi4Bus.readRsp.data.asUInt

    axi4Bus
  }
}

object MemBus {
  def getApb3Config(config: MemBusConfig) = Apb3Config(
    addressWidth = config.addressWidth,
    dataWidth = config.dataWidth,
    selWidth = 1,
    useSlaveError = false
  )

  def getAxi4Config(config: MemBusConfig) = Axi4Config(
    addressWidth = config.addressWidth,
    dataWidth = config.dataWidth,
    useId = true,
    idWidth = config.idWidth,
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

  currentCmd.cmd.id.assignDontCare()

  def isReady: Bool = {
    !currentCmd.isIssued
  }

  bus.cmd.valid := currentCmd.valid
  bus.cmd.id := currentCmd.cmd.id
  bus.cmd.address := currentCmd.cmd.address

  if (bus.config.readWrite) {
    bus.cmd.write := currentCmd.cmd.write
    bus.cmd.wdata := currentCmd.cmd.wdata
    bus.cmd.wmask := currentCmd.cmd.wmask
  }

  bus.rsp.ready := True

  when(bus.cmd.valid && bus.cmd.ready) {
    currentCmd.ready := True
    currentCmd.valid := False
  }

  when(bus.rsp.valid) {
    currentCmd.ready := False
    currentCmd.valid := False
  }

  private def issueCommand(
      address: UInt,
      write: Boolean = false,
      wdata: UInt = null,
      wmask: Bits = null
  ) = {
    assert(!write || bus.config.readWrite)

    when(bus.cmd.ready) {
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

  def invalidate(): Unit = {
    bus.cmd.valid := False
    currentCmd.valid := False
    currentCmd.ready := False
  }

  def read(address: UInt): (Bool, UInt) = {
    val valid = False
    val rdata = U(0, config.xlen bits)
    val dropRsp = False
    val issuedThisCycle = False

    when(!currentCmd.isIssued) {
      issueCommand(address)
      issuedThisCycle := True
    } elsewhen ((currentCmd.cmd.address >> log2Up(config.memBusWidth / 8))
      =/= (address >> log2Up(config.memBusWidth / 8))) {
      dropRsp := True
    }

    when(bus.rsp.valid) {
      currentCmd.valid := False
      currentCmd.ready := False

      when(issuedThisCycle || (!dropRsp && !currentCmd.isWrite)) {
        valid := True
        val addressOffset: Int = log2Up(config.memBusWidth / 8) - 1
        val byteOffset: Int = log2Up(config.xlen / 8)
        val offset: UInt = address(addressOffset downto byteOffset)
        val shifted: Int = log2Up(config.xlen)
        val startingBit: UInt = offset << shifted
        rdata := (bus.rsp.rdata >> startingBit) (config.xlen - 1 downto 0)
      }
    }

    (valid, rdata)
  }

  def write(address: UInt, wdata: UInt, wmask: Bits): Bool = {
    assert(bus.config.readWrite)

    val accepted = False
    val dropRsp = False
    val issuedThisCycle = False

    when(!currentCmd.isIssued) {
      issueCommand(address, write = true, wdata, wmask)
      issuedThisCycle := True
    } elsewhen (currentCmd.cmd.address =/= address) {
      dropRsp := True
    }

    when(bus.cmd.ready) {
      currentCmd.valid := False
      currentCmd.ready := False

      when(issuedThisCycle || (!dropRsp && currentCmd.isWrite)) {
        accepted := True
      }
    }

    accepted
  }
}
