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
          stage.arbitration.isDone :=
            stage.arbitration.isReady && !stage.arbitration.isStalled
        }
      }

      stages.head.arbitration.isValid := True
      stages.head.arbitration.isValid.allowOverride

      for ((stage, nextStage) <- stages.zip(stages.tail)) {
        stage.arbitration.isStalled := nextStage.arbitration.isStalled
      }

      stages.last.arbitration.isStalled := False

      for ((prevStage, stage) <- stages.zip(stages.tail)) {
        val isValidReg = Reg(Bool()).init(False)
        stage.arbitration.isValid := isValidReg

        when (stage.arbitration.isDone || !stage.arbitration.isValid) {
          isValidReg := prevStage.arbitration.isDone
        }
      }

      for ((stage, regs) <- pipelineRegs) {
        regs.shift := stage.arbitration.isDone
      }
    }
  }
}
