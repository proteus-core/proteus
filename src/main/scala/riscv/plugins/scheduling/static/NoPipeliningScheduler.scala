package riscv.plugins.scheduling.static

import riscv._

import spinal.core._
import spinal.lib._

class NoPipeliningScheduler(implicit config: Config) extends Scheduler {
  override def build(pipeline: StaticPipeline): Unit = {
    super.build(pipeline)
    
    pipeline plug new Area {
      import pipeline._

      stages.head.arbitration.isValid := !stages.tail.map(_.arbitration.isValid).orR
    }
  }
}
