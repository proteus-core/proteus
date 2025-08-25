package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class SpeculationTracking(implicit config: Config)
    extends Plugin[Pipeline]
    with SpeculationService {

  object SpeculationTracking {
    object CF_SPECULATIVE extends PipelineData(Bool())
    object MD_SPECULATIVE extends PipelineData(Bool())
    object SPECULATIVE_DEP extends PipelineData(Flow(UInt(log2Up(config.robEntries) bits)))
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          SpeculationTracking.CF_SPECULATIVE -> False,
          SpeculationTracking.MD_SPECULATIVE -> False
        )
      )
    }
  }

  override def speculativeCFMap(): Map[PipelineData[_ <: Data], Bool] = {
    Map(SpeculationTracking.CF_SPECULATIVE -> True)
  }

  override def isSpeculativeCFOutput(stage: Stage): Bool = {
    stage.output(SpeculationTracking.CF_SPECULATIVE)
  }

  override def isSpeculativeCFInput(stage: Stage): Bool = {
    stage.input(SpeculationTracking.CF_SPECULATIVE)
  }

  override def isSpeculativeCF(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](SpeculationTracking.CF_SPECULATIVE.asInstanceOf[PipelineData[Data]])
  }

  override def addIsSpeculativeCF(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      SpeculationTracking.CF_SPECULATIVE.asInstanceOf[PipelineData[Data]],
      SpeculationTracking.CF_SPECULATIVE.dataType
    )
  }

  override def isSpeculativeMDOutput(stage: Stage): Bool = {
    stage.output(SpeculationTracking.MD_SPECULATIVE)
  }

  override def isSpeculativeMDInput(stage: Stage): Bool = {
    stage.input(SpeculationTracking.MD_SPECULATIVE)
  }

  override def isSpeculativeMD(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](SpeculationTracking.MD_SPECULATIVE.asInstanceOf[PipelineData[Data]])
  }

  override def addIsSpeculativeMD(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      SpeculationTracking.MD_SPECULATIVE.asInstanceOf[PipelineData[Data]],
      SpeculationTracking.MD_SPECULATIVE.dataType
    )
  }

  override def speculationDependency(
      bundle: Bundle with DynBundleAccess[PipelineData[Data]]
  ): Flow[UInt] = {
    bundle.elementAs[Flow[UInt]](
      SpeculationTracking.SPECULATIVE_DEP.asInstanceOf[PipelineData[Data]]
    )
  }

  override def addSpeculationDependency(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      SpeculationTracking.SPECULATIVE_DEP.asInstanceOf[PipelineData[Data]],
      SpeculationTracking.SPECULATIVE_DEP.dataType
    )
  }
}
