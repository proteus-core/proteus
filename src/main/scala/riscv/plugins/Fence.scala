package riscv.plugins

import riscv._
import spinal.core._

class Fence(fenceStages: Set[Stage]) extends Plugin[Pipeline] with FenceService {

  object Data {
    object FENCE extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    val issuer = pipeline.service[IssueService]
    issuer.setDestinations(Opcodes.FENCE, fenceStages)

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          Data.FENCE -> False
        )
      )

      config.addDecoding(
        Opcodes.FENCE,
        InstructionType.I,
        Map(
          Data.FENCE -> True
        )
      )
    }
  }

  override def isFence(stage: Stage): Bool = {
    stage.output(Data.FENCE)
  }
}
