package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

/**
  * This abstract class can be used to implement different branch predictors.
  * Derived classes should implement the logic of `predictorComponent`.
  * This component is connected to the fetch stage through `predictIo`
  * and to the jump stage through `jumpIo`.
  *
  * PredictIo can be used by the predictor to set the predicted PC in the fetch stage.
  * PredictIo.currentPc always contains the PC in the fetch stage. If a valid prediction
  * can be made, the predictor should set PredictIo.predictedAddress, which will update the next
  * PC in the fetch stage.
  * If no prediction is given, the predicted PC is set to equal the next PC.
  *
  * JumpIo can be used by the predictor to record jump targets. JumpIo.valid is high
  * whenever a misprediction is detected (either because of a mispredicted jump target,
  * an instruction that had no associated prediction / was not predicted to jump but did,
  * or an instruction that was predicted to jump but did not).
  * In this case, JumpIo.currentPc will contain the
  * value of the PC in the execute stage (belonging to the mispredicted instruction),
  * and JumpIo.target will contain the correct jump target (or the NEXT_PC, in case no jump
  * was taken).
  */
abstract class BranchTargetPredictorBase(fetchStage: Stage, jumpStage: Stage)
  extends Plugin[Pipeline] with BranchTargetPredictorService {

  case class PredictIo() extends Bundle with IMasterSlave {
    val currentPc = UInt(config.xlen bits)
    val predictedAddress = Flow(UInt(config.xlen bits))

    override def asMaster(): Unit = {
      in(currentPc)
      out(predictedAddress)
    }
  }

  class FetchArea extends Area {
    val predictIo: PredictIo = slave(PredictIo())
  }

  case class JumpIo() extends Bundle with IMasterSlave {
    val valid = Bool()
    val currentPc = UInt(config.xlen bits)
    val target = UInt(config.xlen bits)

    override def asMaster(): Unit = {
      out(valid, currentPc, target)
    }
  }

  class JumpArea extends Area {
    val jumpIo: JumpIo = master(JumpIo())
  }

  class PredictorComponent extends Component {
    val predictIo: PredictIo = master(PredictIo())
    val jumpIo: JumpIo = slave(JumpIo())
  }

  private var fetchArea: FetchArea = null
  private var jumpArea: JumpArea = null
  var predictorComponent: PredictorComponent

  private object Data {
    object PREDICTED_PC extends PipelineData(UInt(config.xlen bits))
  }

  override def getPredictedPc(stage: Stage): UInt = {
    stage.output(Data.PREDICTED_PC)
  }

  override def setPredictedPc(stage: Stage, pc: UInt): Unit = {
    stage.input(Data.PREDICTED_PC) := pc
  }

  override def setup(): Unit = {
    jumpArea = jumpStage plug new JumpArea {
      import jumpStage._

      val jumpService = pipeline.getService[JumpService]

      jumpIo.valid := False
      jumpIo.currentPc := output(pipeline.data.PC)
      jumpIo.target := output(pipeline.data.NEXT_PC)

      jumpService.onJump { (stage, _, nextPc, jumpType) =>
        jumpType match {
          case JumpType.Normal =>
            when (predictionWasCorrect(nextPc, getPredictedPc(stage))) {
              // cancel jump if it was correctly predicted in the fetch stage
              pipeline.getService[JumpService].disableJump(stage)
            }
          case _ =>
        }
      }

      // in case of an incorrect prediction, issue a jump to the correct target
      // and notify the predictor
      when (arbitration.isDone &&
            !predictionWasCorrect(value(pipeline.data.NEXT_PC), value(Data.PREDICTED_PC))) {
        jumpIo.valid := True
        jumpIo.currentPc := value(pipeline.data.PC)
        jumpIo.target := value(pipeline.data.NEXT_PC)

        // HACK this forces the jump service to restart the pipeline from NEXT_PC
        jumpService.jumpRequested(jumpStage) := True
      }
    }
  }

  override def build(): Unit = {
    fetchArea = fetchStage plug new FetchArea {
      predictIo.currentPc := fetchStage.value(pipeline.data.PC)

      // set the PREDICTED_PC pipeline register to the prediction if it exists,
      // otherwise set it to NEXT_PC
      // this is necessary because this register is used later on to determine whether
      // the correct next instruction was fetched
      when (predictIo.predictedAddress.valid) {
        fetchStage.output(Data.PREDICTED_PC) := predictIo.predictedAddress.payload
      } otherwise {
        fetchStage.output(Data.PREDICTED_PC) := fetchStage.value(pipeline.data.NEXT_PC)
      }
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      predictorComponent.predictIo <> fetchArea.predictIo
      predictorComponent.jumpIo <> jumpArea.jumpIo

      val jumpService = pipeline.getService[JumpService]

      // if a target was predicted, set the PC of the fetch stage to the prediction
      when (predictorComponent.predictIo.predictedAddress.valid && fetchStage.arbitration.isDone) {
        jumpService.setFetchPc(predictorComponent.predictIo.predictedAddress.payload)
      }
    }
  }

  def predictionWasCorrect(nextPc: UInt, predictedPc: UInt): Bool = {
    nextPc === predictedPc
  }
}
