package riscv.plugins.scheduling.static

import riscv._

import spinal.core._

import scala.collection.mutable

class PcManager extends Plugin[StaticPipeline] with JumpService {
  private val jumpStages = mutable.Set[Stage]()
  private val pcUpdateObservers = mutable.Buffer[PcUpdateObserver]()
  private val jumpObservers = mutable.Buffer[PcUpdateObserver]()

  override def onPcUpdate(observer: PcUpdateObserver): Unit = {
    pcUpdateObservers += observer
  }

  override def onJump(observer: PcUpdateObserver): Unit = {
    jumpObservers += observer
  }

  override def jump(stage: Stage, target: UInt, isTrap: Boolean): Unit = {
    jumpStages += stage

    def doJump() = {
      stage.output(pipeline.data.NEXT_PC) := target
      stage.arbitration.jumpRequested := True
    }

    if (isTrap) {
      doJump()
    } else {
      when (target(1 downto 0) =/= 0) {
        val trapHandler = pipeline.getService[TrapService]
        trapHandler.trap(stage, TrapCause.InstructionAddressMisaligned(target))
      }.otherwise {
        doJump()
      }
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val pc = Reg(UInt(config.xlen bits)).init(0)
      val fetchStage = pipeline.stages.head

      def updatePc(stage: Stage, isJump: Boolean) = {
        val currPc = stage.output(pipeline.data.PC)
        val nextPc = stage.output(pipeline.data.NEXT_PC)
        pc := nextPc

        pcUpdateObservers.foreach(_(stage, currPc, nextPc))

        if (isJump) {
          jumpObservers.foreach(_(stage, currPc, nextPc))
        }
      }

      when (fetchStage.arbitration.isDone) {
        updatePc(fetchStage, false)
      }

      fetchStage.input(pipeline.data.PC) := pc

      for (stage <- jumpStages) {
        when (stage.arbitration.isDone && stage.arbitration.jumpRequested) {
          for (prevStage <- pipeline.stages.takeWhile(_ != stage)) {
            prevStage.arbitration.isValid := False
          }

          updatePc(stage, true)
        }
      }
    }
  }
}
