package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class DataSpeculationTracking(implicit config: Config)
    extends Plugin[Pipeline]
    with DataSpeculationService {

  object DataSpeculationTracking {
    object SSB_SPECULATIVE extends PipelineData(Bool())
    object PSF_SPECULATIVE extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          DataSpeculationTracking.PSF_SPECULATIVE -> False,
          DataSpeculationTracking.SSB_SPECULATIVE -> False
        )
      )
    }
  }

  override def isSsbSpeculative(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](DataSpeculationTracking.SSB_SPECULATIVE.asInstanceOf[PipelineData[Data]])
  }

  override def addIsSsbSpeculative(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      DataSpeculationTracking.SSB_SPECULATIVE.asInstanceOf[PipelineData[Data]],
      DataSpeculationTracking.SSB_SPECULATIVE.dataType
    )
  }

  override def isPsfSpeculative(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](DataSpeculationTracking.PSF_SPECULATIVE.asInstanceOf[PipelineData[Data]])
  }

  override def addIsPsfSpeculative(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      DataSpeculationTracking.PSF_SPECULATIVE.asInstanceOf[PipelineData[Data]],
      DataSpeculationTracking.PSF_SPECULATIVE.dataType
    )
  }
}
