package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class SecDmp extends Plugin[Pipeline] with SecDmpService {
  override def setup(): Unit = {
    assert(
      pipeline.hasService[MemoryPartitioningService],
      "Memory partitioning required for SecDMP"
    )
  }
}
