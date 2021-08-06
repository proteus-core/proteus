package riscv.plugins.scheduling.static

import riscv._

import spinal.core._
import spinal.lib._

import collection.mutable

class Scheduler(canStallExternally: Boolean = false)
  extends Plugin[StaticPipeline] with ScheduleService with IssueService {
  private class ClaimPipelineArea extends Area {
    val claimPipeline = out(Bool())
    val pipelineFlushed = in(Bool())
    claimPipeline := False
  }

  private val claimPipelineAreas = mutable.Map[Stage, ClaimPipelineArea]()

  override def build(): Unit = {
    pipeline plug new Area {
      val stages = pipeline.stages
      stages.head.arbitration.isValid := !ClockDomain.current.readResetWire
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
    }
  }

  override def claimPipeline(stage: Stage): Bool = {
    val area = claimPipelineAreas.getOrElseUpdate(stage, {
      val stageArea = stage plug new ClaimPipelineArea

      pipeline plug new Area {
        val laterStagesEmpty = if (stage == pipeline.retirementStage) {
          True
        } else {
          val laterStages = pipeline.stages.dropWhile(_ != stage).tail
          !laterStages.map(_.arbitration.isValid).orR
        }

        stageArea.pipelineFlushed := laterStagesEmpty

        when (stageArea.claimPipeline) {
          for (prevStage <- pipeline.stages.takeWhile(_ != stage)) {
            prevStage.arbitration.isValid := False
          }

          pipeline.getService[JumpService].setFetchPc(stage.input(pipeline.data.NEXT_PC))
        }
      }

      stageArea
    })

    area.claimPipeline := True
    area.pipelineFlushed
  }

  override def setDestinations(opcode: MaskedLiteral, stages: Set[Stage]): Unit = {
    // Dummy implementation of IssueService
  }
}
