package riscv.plugins.scheduling.static

import riscv._

import spinal.core._

class Scheduler(canStallExternally: Boolean = false)
  extends Plugin[StaticPipeline] with IssueService {
  override def build(): Unit = {
    pipeline plug new Area {
      val stages = pipeline.stages
      stages.head.arbitration.isValid := True
      stages.head.arbitration.isValid.allowOverride

      for ((stage, nextStage) <- stages.zip(stages.tail)) {
        stage.arbitration.isStalled :=
          nextStage.arbitration.isStalled ||
          (nextStage.arbitration.isValid && !nextStage.arbitration.isReady)
      }

      if (!canStallExternally) {
        stages.last.arbitration.isStalled := False
      }

      for ((prevStage, stage) <- stages.zip(stages.tail)) {
        val isValidReg = Reg(Bool()).init(False)
        stage.arbitration.isValid := isValidReg

        when (stage.arbitration.isDone || !stage.arbitration.isValid) {
          isValidReg := prevStage.arbitration.isDone
        }
      }

      for ((stage, regs) <- pipeline.pipelineRegs) {
        regs.shift := stage.arbitration.isDone
      }

      stages.head.input(pipeline.data.PC) :=
        stages.head.output(pipeline.data.NEXT_PC)
      stages.head.input(pipeline.data.PC).allowOverride
    }
  }

  override def setDestinations(opcode: MaskedLiteral, stages: Set[Stage]): Unit = {
    // Dummy implementation of IssueService
  }
}
