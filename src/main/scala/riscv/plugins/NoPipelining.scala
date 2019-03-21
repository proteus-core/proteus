package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

class NoPipelining extends SimplePipelining {
  override def build(pipeline: Pipeline, config: Config): Unit = {
    super.build(pipeline, config)
    
    pipeline plug new Area {
      import pipeline._

      stages.head.arbitration.isValid := !stages.tail.map(_.arbitration.isValid).orR
    }
  }
}
