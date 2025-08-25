package riscv.plugins

import riscv._
import spinal.core._

class NoPredictionPredictor(fetchStage: Stage, executeStage: Stage)
    extends Plugin[Pipeline]
    with BranchTargetPredictorService {
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

  override def predictedPc(stage: Stage): UInt = {
    stage.output(Data.PREDICTED_PC)
  }

  override def setPredictedPc(stage: Stage, pc: UInt): Unit = {
    stage.input(Data.PREDICTED_PC) := pc
  }

  override def predictedPc(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): UInt =
    bundle.elementAs[UInt](Data.PREDICTED_PC.asInstanceOf[PipelineData[Data]])

  override def preventFlush(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Unit = {}
}
