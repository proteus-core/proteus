package riscv.plugins

import riscv._
import spinal.core._

class RegisterFence extends Plugin[DynamicPipeline] with RegisterFenceService {

  val opcode = M"0001-------------000-----0001111"

  object Data {
    object REGFENCE extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    val issuer = pipeline.service[IssueService]
    issuer.setDestinations(opcode, pipeline.rsStages.toSet)

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          Data.REGFENCE -> False
        )
      )

      config.addDecoding(
        opcode,
        InstructionType.I,
        Map(
          Data.REGFENCE -> True
        )
      )
    }
  }

  override def build(): Unit = {
    for (stage <- pipeline.rsStages) {
      stage plug new Area {
        when(stage.input(Data.REGFENCE)) {
          stage.output(pipeline.data.RD_DATA) := stage.input(pipeline.data.RS1_DATA)
          stage.output(pipeline.data.RD_DATA_VALID) := True
        }
      }
    }
  }

  override def isRegisterFence(stage: Stage): Bool = {
    stage.output(Data.REGFENCE)
  }
}
