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

  def predictionWasCorrect(pc: UInt, nextPc: UInt, predictedPc: UInt): Bool = {
    nextPc === predictedPc
  }

  override def addPredictedPcToBundle(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      Data.PREDICTED_PC.asInstanceOf[PipelineData[Data]],
      Data.PREDICTED_PC.dataType
    )
  }

  override def predictedPcOfBundle(
      bundle: Bundle with DynBundleAccess[PipelineData[Data]]
  ): UInt = {
    bundle.elementAs[UInt](Data.PREDICTED_PC.asInstanceOf[PipelineData[Data]])
  }
}
