package riscv.plugins.capabilities

import riscv._

import spinal.core._

class GlobalPipelineData(pipeline: Pipeline)(implicit context: Context) {
  object PCC extends PipelineData(NonPointerCapability())

  // CS1, CS2, and CD are in the same position in the encodings as RS1, RS2, and
  // RD, respectively. Therefore, we just reuse those PipelineData objects to
  // 1) save pipeline registers and 2) simplify decoding.
  val CS1 = pipeline.data.RS1
  object CS1_DATA extends PipelineData(Capability())
  val CS2 = pipeline.data.RS2
  object CS2_DATA extends PipelineData(Capability())
  val CD = pipeline.data.RD
  object CD_DATA extends PipelineData(Capability())
}
