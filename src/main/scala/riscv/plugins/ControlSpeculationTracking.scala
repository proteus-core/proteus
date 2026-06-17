package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class ControlSpeculationTracking(implicit config: DynamicPipelineConfig)
    extends Plugin[DynamicPipeline]
    with ControlSpeculationService {

  object ControlSpeculationTracking {
    object CF_SPECULATIVE extends PipelineData(Bool())
    object SPECULATIVE_DEP extends PipelineData(Flow(UInt(log2Up(config.robEntries) bits)))
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          ControlSpeculationTracking.CF_SPECULATIVE -> False
        )
      )
    }
  }

  override def speculativeCFMap(): Map[PipelineData[_ <: Data], Bool] = {
    Map(ControlSpeculationTracking.CF_SPECULATIVE -> True)
  }

  override def isSpeculativeCFOutput(stage: Stage): Bool = {
    stage.output(ControlSpeculationTracking.CF_SPECULATIVE)
  }

  override def isSpeculativeCFInput(stage: Stage): Bool = {
    stage.input(ControlSpeculationTracking.CF_SPECULATIVE)
  }

  override def isSpeculativeCF(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](
      ControlSpeculationTracking.CF_SPECULATIVE.asInstanceOf[PipelineData[Data]]
    )
  }

  override def addIsSpeculativeCF(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      ControlSpeculationTracking.CF_SPECULATIVE.asInstanceOf[PipelineData[Data]],
      ControlSpeculationTracking.CF_SPECULATIVE.dataType
    )
  }

  override def speculationDependency(
      bundle: Bundle with DynBundleAccess[PipelineData[Data]]
  ): Flow[UInt] = {
    bundle.elementAs[Flow[UInt]](
      ControlSpeculationTracking.SPECULATIVE_DEP.asInstanceOf[PipelineData[Data]]
    )
  }

  override def addSpeculationDependency(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      ControlSpeculationTracking.SPECULATIVE_DEP.asInstanceOf[PipelineData[Data]],
      ControlSpeculationTracking.SPECULATIVE_DEP.dataType
    )
  }
}
