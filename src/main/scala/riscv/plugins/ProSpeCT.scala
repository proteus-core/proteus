package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

object ProSpeCTData {
  // the instruction may leak its operands (not in InstructionClass.safe)
  object UNSAFE extends PipelineData(Bool())
}

class ProSpeCT extends Plugin[DynamicPipeline] with ProSpeCTService {
  override def setup(): Unit = {
    assert(
      pipeline.hasService[PipelineTaintService],
      "Taint tracking required for ProSpeCT"
    )
    assert(
      pipeline.hasService[ControlSpeculationService],
      "Control speculation tracking required for ProSpeCT"
    )
    assert(
      pipeline.hasService[DataSpeculationService],
      "Data speculation tracking required for ProSpeCT"
    )
    assert(
      pipeline.hasService[MemoryPartitioningService],
      "Memory partitioning required for ProSpeCT"
    )

    // every instruction is unsafe unless whitelisted
    pipeline.service[DecoderService].configure { decoderConfig =>
      decoderConfig.addDefault(ProSpeCTData.UNSAFE, True)
      for (op <- InstructionClass.safe(pipeline.config)) {
        decoderConfig.addDecoding(op, Map(ProSpeCTData.UNSAFE -> False))
      }
    }

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
