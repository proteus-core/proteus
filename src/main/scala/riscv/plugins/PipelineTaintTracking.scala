package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class PipelineTaintTracking(implicit config: Config)
    extends Plugin[DynamicPipeline]
    with PipelineTaintService {
  private var registerTaints: Vec[Bool] = null

  private object Taint {
    object TAINTED_VALUE extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          Taint.TAINTED_VALUE -> False
        )
      )
    }
  }

  override def build(): Unit = {
    // force taint to be propagated, might not be needed
    tainted(pipeline.retirementStage)

    pipeline plug new Area {
      val regArray = Vec.fill(config.numRegs)(RegInit(False))
      registerTaints = regArray
    }
  }

  override def tainted(stage: Stage): Bool = {
    stage.output(Taint.TAINTED_VALUE)
  }

  override def taintedPipelineReg(reg: PipelineData[Data]): Boolean = {
    reg == Taint.TAINTED_VALUE
  }

  override def tainted(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](Taint.TAINTED_VALUE.asInstanceOf[PipelineData[Data]])
  }

  override def addTaintToBundle(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      Taint.TAINTED_VALUE.asInstanceOf[PipelineData[Data]],
      Taint.TAINTED_VALUE.dataType
    )
  }

  override def registerTaint(regId: UInt): Bool = {
    registerTaints(regId)
  }

}
