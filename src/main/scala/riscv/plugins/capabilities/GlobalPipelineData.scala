package riscv.plugins.capabilities

import riscv._

import spinal.core._

class GlobalPipelineData(pipeline: Pipeline)(implicit context: Context) {
  object PCC extends PipelineData(PackedCapability(hasOffset = false))
  object CS1_DATA extends PipelineData(RegCapability())
  object CS2_DATA extends PipelineData(RegCapability())
  object CD_DATA extends PipelineData(RegCapability())
}
