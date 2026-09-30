package riscv.plugins

import riscv._
import spinal.core._

class DeclassificationChecker extends Plugin[DynamicPipeline] with DeclassificationCheckerService {
  override def setup(): Unit = {
    assert(
      pipeline.hasService[PipelineTaintService],
      "Taint tracking required for DeclassificationChecker"
    )
    assert(
      pipeline.hasService[MemoryPartitioningService],
      "Memory partitioning required for DeclassificationChecker"
    )

    pipeline
      .service[LsuService]
      .setAddressTranslator(new LsuAddressTranslator {
        override def translate(
            stage: Stage,
            address: UInt,
            operation: SpinalEnumCraft[LsuOperationType.type],
            width: SpinalEnumCraft[LsuAccessWidth.type]
        ): UInt = {
          pipeline
            .service[PipelineTaintService]
            .tainted(stage) := (operation === LsuOperationType.LOAD && pipeline
            .service[MemoryPartitioningService]
            .isInPartition(isStage = true, address))
          address
        }
      })
  }
}
