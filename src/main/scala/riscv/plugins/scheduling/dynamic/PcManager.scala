package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Flow

class PcManager extends Plugin[DynamicPipeline] with JumpService {
  override def jump(stage: Stage, target: UInt, jumpType: JumpType, checkAlignment: Boolean): Unit = {
    stage.output(pipeline.data.NEXT_PC) := target
    stage.output(pipeline.data.JUMP_REQUESTED) := True
  }

  // TODO: should the following functions have a body? would it maybe make sense to inherit from PcManager and use those? decent amount of duplication already
  override def addPcPayload[T <: Data](pcPayload: PcPayload[T]): Unit = ???

  override def onPcUpdate(observer: PcUpdateObserver): Unit = ???

  override def onJump(observer: JumpObserver): Unit = ???

  override def jump(target: UInt): Unit = ???

  override def jumpRequested(stage: Stage): Bool = {
    stage.output(pipeline.data.JUMP_REQUESTED)
  }

  override def setFetchPc(pc: UInt): Unit = ???

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure {config =>
      config.addDefault(pipeline.data.JUMP_REQUESTED, False)
    }

    // Make sure the needed pipeline registers are correctly propagated. Needed because we use
    // them during the finish phase.
    pipeline.retirementStage.output(pipeline.data.JUMP_REQUESTED)
    pipeline.retirementStage.output(pipeline.data.NEXT_PC)
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val jumpStage = pipeline.retirementStage

      when (jumpStage.arbitration.isDone && jumpRequested(jumpStage)) {
        val staticPcManager = pipeline.issuePipeline.getService[JumpService]
        staticPcManager.jump(jumpStage.output(pipeline.data.NEXT_PC))

        for (exeStage <- pipeline.exeStages) {
          exeStage.arbitration.isValid := False
        }
      }
    }
  }
}
