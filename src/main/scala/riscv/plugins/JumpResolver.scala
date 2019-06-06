package riscv.plugins

import riscv._

import spinal.core._

import scala.collection.mutable

class JumpResolver(implicit config: Config) extends Plugin with JumpService {
  private val jumpStages = mutable.Set[Stage]()

  override def jump(pipeline: Pipeline, stage: Stage, target: UInt): Unit = {
    stage.output(pipeline.data.NEXT_PC) := target
    stage.arbitration.jumpRequested := True
    jumpStages += stage
  }

  override def finish(pipeline: Pipeline): Unit = {
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
