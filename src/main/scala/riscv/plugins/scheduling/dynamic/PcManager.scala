package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Flow

import scala.collection.mutable

class PcManager extends Plugin[DynamicPipeline] with JumpService {
  private var globalTarget: Flow[UInt] = null
  private val jumpObservers = mutable.Buffer[JumpObserver]()

  override def jump(stage: Stage, target: UInt, jumpType: JumpType, checkAlignment: Boolean): Unit = {
    stage.output(pipeline.data.NEXT_PC) := target
    stage.arbitration.jumpRequested := True

    jumpObservers.foreach(_(stage, stage.value(pipeline.data.PC), target, jumpType))

    // TODO: jump in static manager?
  }

  // TODO: should the following functions have a body? would it maybe make sense to inherit
  //  from PcManager and use those? decent amount of duplication already
  override def addPcPayload[T <: Data](pcPayload: PcPayload[T]): Unit = ???

  override def onPcUpdate(observer: PcUpdateObserver): Unit = ???

  override def onJump(observer: JumpObserver): Unit = {
    jumpObservers += observer
  }

  override def jump(target: UInt): Unit = {
    globalTarget.push(target)
  }

  override def setFetchPc(pc: UInt): Unit = {
    val staticPcManager = pipeline.issuePipeline.getService[JumpService]
    staticPcManager.setFetchPc(pc)
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      globalTarget = Flow(UInt(config.xlen bits))
      globalTarget.valid := False
      globalTarget.payload.assignDontCare()
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      // TODO: invalidate other exeStages when non-global jump?
      when (globalTarget.valid) {
        val staticPcManager = pipeline.issuePipeline.getService[JumpService]
        staticPcManager.jump(globalTarget.payload)

        for (exeStage <- pipeline.exeStages) {
          exeStage.arbitration.isValid := False
        }
      }
    }
  }

  override def disableJump(stage: Stage): Unit = {
    val staticPcManager = pipeline.issuePipeline.getService[JumpService]
    staticPcManager.disableJump(stage)
  }
}

