package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

class NoPipelining(implicit config: Config) extends SimplePipelining {
  override def build(pipeline: Pipeline): Unit = {
    super.build(pipeline)
    
    pipeline plug new Area {
      import pipeline._

      stages.head.arbitration.isValid := !stages.tail.map(_.arbitration.isValid).orR
    }
  }
}
