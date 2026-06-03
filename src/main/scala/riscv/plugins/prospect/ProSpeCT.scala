package riscv.plugins.prospect

import riscv._
import spinal.core._

class ProSpeCT extends Plugin[DynamicPipeline] with ProSpeCTService {
  override def setup(): Unit = {
    assert(
      pipeline.hasService[MemoryTaggerService] || pipeline.hasService[SecretRegionService],
      "Secret tracking in memory required for ProSpeCT"
    )
    assert(pipeline.hasService[PipelineTaintService], "Taint tracking required for ProSpeCT")
    assert(
      pipeline.hasService[ControlSpeculationService],
      "Control speculation tracking required for ProSpeCT"
    )
    assert(
      pipeline.hasService[DataSpeculationService],
      "Data speculation tracking required for ProSpeCT"
    )
  }
}
