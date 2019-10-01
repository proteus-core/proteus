package riscv.plugins.scheduling.static

import riscv._

import spinal.core._

import scala.collection.mutable

class JumpResolver(implicit config: Config) extends Plugin[StaticPipeline] with JumpService {
  private val jumpStages = mutable.Set[Stage]()

  override def jump(pipeline: Pipeline, stage: Stage,
                    target: UInt, isTrap: Boolean): Unit = {
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
        trapHandler.trap(pipeline, stage,
                         TrapCause.InstructionAddressMisaligned(target))
      }.otherwise {
        doJump()
      }
    }
  }

  override def finish(pipeline: StaticPipeline): Unit = {
    pipeline plug new Area {
      val nextPc = Reg(UInt(config.xlen bits)).init(0)

      when (pipeline.fetch.arbitration.isDone) {
        nextPc := pipeline.fetch.output(pipeline.data.NEXT_PC)
      }

      pipeline.fetch.input(pipeline.data.NEXT_PC) := nextPc

      for (stage <- jumpStages) {
        when (stage.arbitration.isDone && stage.arbitration.jumpRequested) {
          for (prevStage <- pipeline.stages.takeWhile(_ != stage)) {
            prevStage.arbitration.isValid := False
          }

          nextPc := stage.output(pipeline.data.NEXT_PC)
        }
      }
    }
  }
}
