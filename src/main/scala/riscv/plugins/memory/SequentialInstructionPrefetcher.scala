package riscv.plugins.memory

import riscv._
import spinal.core._

class SequentialInstructionPrefetcher(implicit config: Config)
    extends Plugin[Pipeline]
    with PrefetchService {

  private var currentAddress: UInt = null
  private val insignificantBits =
    log2Up(config.isa.xlen / 8) + log2Up(config.memBusWidth / config.isa.xlen)
  private var hasNewTarget: Bool = null

  override def setup(): Unit = {
    pipeline plug new Area {
      currentAddress = RegInit(UInt(config.isa.xlen bits).getZero)
      hasNewTarget = Reg(Bool()).init(False)
    }
  }

  override def notifyLoadRequest(address: UInt): Unit = {
    when(currentAddress >> insignificantBits =/= address >> insignificantBits) {
      currentAddress := address
      hasNewTarget := True
    }
  }

  override def notifyLoadResponseFromMemory(address: UInt, data: UInt): Unit = {}

  override def notifyPrefetchResponseFromMemory(address: UInt, data: UInt, id: UInt): Unit = {}

  override def getNextPrefetchTarget(id: UInt): UInt = {
    hasNewTarget := False
    ((currentAddress >> insignificantBits) + 1) << insignificantBits
  }

  override def hasPrefetchTarget: Bool = {
    hasNewTarget
  }
}
