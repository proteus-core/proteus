package riscv.plugins.memory

import riscv._
import spinal.core._

class SequentialInstructionPrefetcher(implicit config: Config) extends Plugin with PrefetchService {

  private var currentAddress: UInt = null
  private val insignificantBits = log2Up(config.xlen / 8) + log2Up(config.memBusWidth / config.xlen)
  private var hasNewTarget: Bool = null

  override def setup(): Unit = {
    pipeline plug new Area {
      currentAddress = RegInit(UInt(config.xlen bits).getZero)
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

  override def notifyPrefetchResponseFromMemory(address: UInt, data: UInt): Unit = {}

  override def getNextPrefetchTarget: UInt = {
    hasNewTarget := False
    ((currentAddress >> insignificantBits) + 1) << insignificantBits
  }

  override def hasPrefetchTarget: Bool = {
    hasNewTarget
  }
}
