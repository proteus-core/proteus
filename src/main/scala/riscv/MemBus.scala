package riscv

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._

case class MemBusConfig(
  addressWidth: Int,
  dataWidth: Int,
  readWrite: Boolean = true
) {
  def byte2WordAddress(ba: UInt): UInt = ba(dataWidth - 1 downto log2Up(dataWidth / 8))
  def word2ByteAddress(wa: UInt): UInt = wa << log2Up(dataWidth / 8)
}

case class MemBusCmd(config: MemBusConfig) extends Bundle {
  val address = UInt(config.addressWidth bits)
  val write = if (config.readWrite) Bool() else null
  val wdata = if (config.readWrite) UInt(config.dataWidth bits) else null
  val wmask = if (config.readWrite) Bits(config.dataWidth / 8 bits) else null
}

case class MemBusRsp(config: MemBusConfig) extends Bundle {
  val rdata = UInt(config.dataWidth bits)
}

class MemBus(val config: MemBusConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(MemBusCmd(config))
  val rsp = Stream(MemBusRsp(config))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

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
    cmd.ready := axi4Bus.readCmd.ready

    rsp.valid := axi4Bus.readRsp.valid
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
    cmd.ready := axi4Bus.sharedCmd.ready

    axi4Bus.writeData.valid := cmd.valid
    axi4Bus.writeData.data := cmd.wdata.asBits
    axi4Bus.writeData.strb := cmd.wmask
    axi4Bus.writeData.last := True

    // TODO: Set useResp to true and verify response?
    axi4Bus.writeRsp.ready := rsp.ready
    axi4Bus.readRsp.ready := rsp.ready
    rsp.valid := axi4Bus.readRsp.valid
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

  def isReady: Bool = {
    !currentCmd.isIssued
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
    val rdata = U(0, bus.config.dataWidth bits)
    val dropRsp = False
    val issuedThisCycle = False

    when (!currentCmd.isIssued) {
      issueCommand(address)
      issuedThisCycle := True
    } elsewhen (currentCmd.cmd.address =/= address) {
      dropRsp := True
    }

    when (bus.rsp.valid) {
      currentCmd.valid := False
      currentCmd.ready := False

      when (issuedThisCycle || (!dropRsp && !currentCmd.isWrite)) {
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
    val issuedThisCycle = False

    when (!currentCmd.isIssued) {
      issueCommand(address, write = true, wdata, wmask)
      issuedThisCycle := True
    } elsewhen (currentCmd.cmd.address =/= address) {
      dropRsp := True
    }

    when (bus.cmd.ready) {
      currentCmd.valid := False
      currentCmd.ready := False

      when (issuedThisCycle || (!dropRsp && currentCmd.isWrite)) {
        accepted := True
      }
    }

    accepted
  }
}

class IBusControl(
   bus: MemBus,
   ibusLatency: Int,
   irQueueSize: Int = 4
 )(implicit config: Config) extends Area {
  assert(!bus.config.readWrite)
  assert(ibusLatency > 0)

  case class Cmd() extends Bundle {
    val address = UInt(bus.config.addressWidth bits)
  }

  case class Rsp() extends Bundle {
    val address = UInt(bus.config.addressWidth bits)
    val ir = UInt(bus.config.dataWidth bits)
  }

  // FIFO that holds the memory commands that are currently in flight (command sent but no response
  // yet). This is used to match incoming responses to the address to which they correspond.
  // We use a StreamFifoLowLatency instead of StreamFifo as the latter has a 2-cycle delay between
  // a push and the value being available to be popped. This causes problems when the latency is
  // only 1 cycle.
  // We use a depth of one more than the latency because the FIFO doesn't allow pushes while full
  // even when we are popping at the same time.
  val cmdFifo = new StreamFifoLowLatency(Cmd(), depth = ibusLatency + 1, latency = 1)
  cmdFifo.io.push.valid := False
  cmdFifo.io.push.payload.assignDontCare()
  cmdFifo.io.pop.ready := False
  cmdFifo.io.flush := False

  // FIFO that holds memory responses that have not been consumed yet. This will only fill when the
  // pipeline stalls.
  val rspFifo = new StreamFifoLowLatency(Rsp(), depth = irQueueSize, latency = 1)
  rspFifo.io.push.valid := False
  rspFifo.io.push.payload.assignDontCare()
  rspFifo.io.pop.ready := False
  rspFifo.io.flush := False

  // The AXI4 spec says that when the valid signal is raised, it should stay high until it is
  // acknowledged by the corresponding ready signal. Since our client might not wait for a response
  // (e.g., the fetch stage gets invalidated due to a jump), we have to buffer incoming commands
  // to make sure we can finish them.
  val currentCmd = new Area {
    val valid = RegInit(False)
    val accepted = RegInit(False)
    val address = Reg(UInt(bus.config.addressWidth bits))
  }

  bus.cmd.payload.address := currentCmd.address
  bus.cmd.valid := currentCmd.valid && !currentCmd.accepted

  // FIXME We always accept responses. This means that responses get dropped when rspFifo is full.
  bus.rsp.ready := True

  // Address to restart fetching from
  val restartAddress = Flow(UInt(bus.config.addressWidth bits))
  restartAddress.valid := False
  restartAddress.payload.assignDontCare()

  // When restarting from a new address, we have to flush all in-flight commands. To make sure we
  // can immediately issue a command for the restarted address, we keep track of how many commands
  // are currently in-flight and simply ignore their responses. The maximum number of in-flight
  // commands is the depth of cmdFifo plus one for nextCmd.
  val restartCmdsToFlush = Reg(UInt(log2Up(cmdFifo.depth + 1) bits)).init(0)
  val restarting = restartCmdsToFlush =/= 0

  // Did we accept restartAddress?
  val restartAccepted = False

  // Next command to be put on the bus
  val nextCmd = Flow(UInt(bus.config.addressWidth bits))
  nextCmd.valid := False
  nextCmd.payload.assignDontCare()

  // Are we popping a value from cmdFifo?
  val poppingCmd  = False

  when (restartAddress.valid) {
    nextCmd.push(restartAddress.payload)
  } elsewhen (currentCmd.valid) {
    // We speculatively prefetch the next word when no new address is requested
    nextCmd.push(currentCmd.address + bus.config.dataWidth / 8)
  }

  when (nextCmd.valid) {
    when (!currentCmd.valid || currentCmd.accepted || bus.cmd.ready) {
      // We start a new command when 1) it is the first command, or 2) the previous command was
      // previously accepted on the bus, or 3) the previous command is accepted during the current
      // cycle.
      currentCmd.address := nextCmd.payload
      currentCmd.valid := True
      currentCmd.accepted := False

      // We accepted a new command which is a restart if restartAddress is valid.
      restartAccepted := restartAddress.valid
    }

    when (!currentCmd.valid || currentCmd.accepted) {
      // When the previous command was already accepted, we immediately put the next one on the bus
      // to prevent a cycle delay.
      bus.cmd.payload.address := nextCmd.payload
      bus.cmd.valid := True

      // Make sure the status of currentCmd is correctly set when the command is immediately
      // accepted.
      currentCmd.accepted := bus.cmd.ready
    }
  } elsewhen (currentCmd.valid && !currentCmd.accepted) {
    // When we don't have a new command, update the current command's status based on the bus.
    currentCmd.accepted := bus.cmd.ready
  }

  when (restartAccepted) {
    // The amount of commands to flush when restarting is the amount currently in cmdFifo, minus one
    // if we are popping from cmdFifo in this cycle, and plus one if currentCmd is valid but not yet
    // accepted (as it will be pushed on cmdFifo later).
    restartCmdsToFlush :=
      cmdFifo.io.occupancy - U(poppingCmd) + U(currentCmd.valid && !currentCmd.accepted)
  }

  // Should a memory response be pushed on rspFifo? When it's accepted immediately by the fetch
  // stage, we don't have to store it.
  val pushRsp = True

  // Store a memory response
  when (bus.rsp.valid) {
    val rsp = Rsp()

    // Pop the address off the cmdFifo
    rsp.address := cmdFifo.io.pop.payload.address
    cmdFifo.io.pop.ready := True
    poppingCmd := True

    // ir is on the bus
    rsp.ir := bus.rsp.payload.rdata

    // Push it only when pushRsp is true
    rspFifo.io.push.payload := rsp
    rspFifo.io.push.valid := pushRsp

    when (restarting) {
      // We just got a response for a command while restarting, one less to flush.
      restartCmdsToFlush := restartCmdsToFlush - 1
    }
  }

  // Push accepted commands on cmdFifo
  when (bus.cmd.valid && bus.cmd.ready) {
    val toPush = (currentCmd.valid && !currentCmd.accepted) ? currentCmd.address | nextCmd.payload
    cmdFifo.io.push.payload.address := toPush
    cmdFifo.io.push.valid := True
  }

  // Return and remove the oldest response. This can be either from rspFifo or directly from the
  // memory bus (in the latter case, it's simply not stored in rspFifo).
  def popRsp(): Flow[Rsp] = {
    val result = Flow(Rsp())
    result.valid := False
    result.payload.assignDontCare()

    when (rspFifo.io.pop.valid) {
      // If there is a response in rspFifo, return and pop it (toFlow sets ready).
      result := rspFifo.io.pop.toFlow
    } elsewhen (bus.rsp.valid) {
      // Otherwise, return the response on the bus if there is one.
      result.payload.address := cmdFifo.io.pop.payload.address
      cmdFifo.io.pop.ready := True
      result.payload.ir := bus.rsp.payload.rdata
      result.valid := True

      // Make sure the response is not pushed on rspFifo.
      pushRsp := False
    }

    when (restarting) {
      // Make sure responses are ignored while restarting. If we don't do this, we can get in a
      // "restart loop" when near forward jumps happen because the restarted address is already
      // in-flight and added again to cmdFifo. When the second response comes in, we have to restart
      // again and this cycle continues until we have a jump to an address that is not yet in
      // cmdFifo.
      result.valid := False
    }

    result
  }

  def read(address: UInt): (Bool, UInt) = {
    val valid = False
    val ir = UInt(config.xlen bits).assignDontCare()
    val restartNeeded = False

    val rsp = popRsp().setPartialName("nextIBusRsp")

    when (rsp.valid) {
      when (rsp.payload.address === address) {
        // We have a valid response that matches the requested address, return it.
        valid := True
        ir := rsp.payload.ir
      } otherwise {
        // We have a valid response that doesn't match the requested address. This means a jump
        // happened and we need to restart fetching from the new address.
        restartNeeded := True
      }
    } elsewhen (!cmdFifo.io.pop.valid || cmdFifo.io.pop.payload.address =/= address) {
      // We don't have any responses and we either 1) don't have any in-flight requests, or 2) the
      // oldest in-flight request is for an address we don't need. Restart fetching from the
      // requested address.
      restartNeeded := True
    }

    when (restartNeeded && !restarting){
      // Request restart from address
      restartAddress.push(address)

      // Clear rspFifo because it doesn't contain anything we can use. Note that for short forward
      // jumps, it might actually contain the requested instruction. Maybe we could optimize here.
      rspFifo.io.flush := True
    }

    (valid, ir)
  }
}
