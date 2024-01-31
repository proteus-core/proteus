package riscv.plugins.memory

import riscv._
import spinal.core._

class SequentialInstructionPrefetcher(implicit config: Config) extends Plugin with PrefetchService {

  private var nextTarget: UInt = null

  override def setup(): Unit = {
    pipeline plug new Area {
      nextTarget = RegInit(UInt(config.xlen bits).getZero)
    }
  }

  override def updatePrefetcherState(address: UInt, insignificantBits: Int): Unit = {
    nextTarget := ((address >> insignificantBits) + 1) << insignificantBits
  }

  override def getPrefetchTarget: UInt = {
    nextTarget
  }
}
