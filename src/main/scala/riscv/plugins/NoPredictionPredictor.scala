package riscv.plugins

import riscv._
import spinal.core._

class NoPredictionPredictor(fetchStage: Stage, executeStage: Stage)
  extends Plugin[Pipeline] with BranchTargetPredictorService {
  private object Data {
    object PREDICTED_PC extends PipelineData(UInt(config.xlen bits))
  }

  override def build(): Unit = {
    fetchStage plug new Area {
      fetchStage.output(Data.PREDICTED_PC) := fetchStage.value(pipeline.data.NEXT_PC)
    }

    executeStage plug new Area {
      executeStage.output(Data.PREDICTED_PC)
    }
  }

  override def getPredictedPc(stage: Stage): UInt = {
    stage.output(Data.PREDICTED_PC)
  }

  def predictionWasCorrect(pc: UInt, nextPc: UInt, predictedPc: UInt): Bool = {
    nextPc === predictedPc
  }
}
