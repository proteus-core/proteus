package riscv.plugins.scheduling.static

import riscv._

import spinal.core._

class Scheduler(implicit config: Config) extends Plugin[StaticPipeline] {
  override def build(pipeline: StaticPipeline): Unit = {
    pipeline plug new Area {
      import pipeline._

      for (stage <- stages) {
        stage plug new Area {
          stage.arbitration.isDone := stage.arbitration.isValid &&
                                      stage.arbitration.isReady &&
                                      !stage.arbitration.isStalled
        }
      }

      stages.head.arbitration.isValid := True
      stages.head.arbitration.isValid.allowOverride

      for ((stage, nextStage) <- stages.zip(stages.tail)) {
        stage.arbitration.isStalled :=
          nextStage.arbitration.isStalled ||
          (nextStage.arbitration.isValid && !nextStage.arbitration.isReady)
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
