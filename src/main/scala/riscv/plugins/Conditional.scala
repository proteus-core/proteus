package riscv.plugins

import riscv._

import spinal.core._

class Conditional(exeStages: Set[Stage]) extends Plugin[Pipeline] {
  object ConditionOp extends SpinalEnum {
    val NONE, EQZ, NEZ = newElement()
  }

  object Data {
    object CONDITION_OP extends PipelineData(ConditionOp())
  }

  override def setup(): Unit = {
    val issuer = pipeline.service[IssueService]

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          Data.CONDITION_OP -> ConditionOp.NONE
        )
      )

      val ops = Seq(
        (Opcodes.CZEROEQZ, ConditionOp.EQZ, InstructionType.R),
        (Opcodes.CZERONEZ, ConditionOp.NEZ, InstructionType.R)
      )

      for ((opcode, cond, itype) <- ops) {
        config.addDecoding(
          opcode,
          itype,
          Map(
            Data.CONDITION_OP -> cond
          )
        )
        issuer.setDestinations(opcode, exeStages)
      }
    }
  }

  override def build(): Unit = {
    for (exeStage <- exeStages) {
      exeStage plug new Area {
        import exeStage._

        val src = UInt(config.xlen bits)
        src := value(pipeline.data.RS1_DATA)
        val cond = UInt(config.xlen bits)
        cond := value(pipeline.data.RS2_DATA)

        val op = value(Data.CONDITION_OP)

        val moveZero = op.mux(
          ConditionOp.NONE -> True,
          ConditionOp.EQZ -> (cond === 0),
          ConditionOp.NEZ -> (cond =/= 0)
        )

        when(arbitration.isValid && op =/= ConditionOp.NONE) {
          arbitration.rs1Needed := True
          arbitration.rs2Needed := True
          output(pipeline.data.RD_DATA) := moveZero ? U(0) | src
          output(pipeline.data.RD_DATA_VALID) := True
        }
      }
    }
  }
}
