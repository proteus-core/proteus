package riscv.plugins.capabilities

import riscv._

import spinal.core._

class GlobalPipelineData(pipeline: Pipeline)(implicit context: Context) {
  object PCC extends PipelineData(NonPointerCapability())
  object CS1_DATA extends PipelineData(Capability())
  object CS2_DATA extends PipelineData(Capability())
  object CD_DATA extends PipelineData(Capability())
}
