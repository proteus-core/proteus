package riscv.plugins

import riscv._

import spinal.core._

class SimplePipelining extends Plugin {
  override def build(pipeline: Pipeline, config: Config): Unit = {
    pipeline plug new Area {
      import pipeline._

      for (stage <- stages) {
        stage plug new Area {
          stage.arbitration.isReady := stage.arbitration.isValid
        }
      }

      stages.head.arbitration.isValid := True
      stages.head.arbitration.isValid.allowOverride

      for ((prevStage, stage) <- stages.zip(stages.tail)) {
        stage.arbitration.isValid := RegNext(prevStage.arbitration.isReady, False)
      }

      for ((stage, regs) <- pipelineRegs) {
        regs.shift := stage.arbitration.isReady
      }
    }
  }
}
