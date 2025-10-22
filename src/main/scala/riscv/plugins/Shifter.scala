package riscv.plugins

import riscv._

import spinal.core._

class Shifter(exeStages: Set[Stage]) extends Plugin[Pipeline] {
  object ShiftOp extends SpinalEnum {
    val NONE, SLL, SRL, SRA = newElement()
  }

  object Data {
    object SHIFT_OP extends PipelineData(ShiftOp())
    object SHIFT_TRUNC extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    val issuer = pipeline.service[IssueService]

    pipeline.service[DecoderService].configure { dconfig =>
      dconfig.addDefault(
        Map(
          Data.SHIFT_OP -> ShiftOp.NONE,
          Data.SHIFT_TRUNC -> False
        )
      )

      val imm_ops = if (config.xlen == 64) {
        Seq(
          (Opcodes64.SLLI, ShiftOp.SLL, InstructionType.I),
          (Opcodes64.SRLI, ShiftOp.SRL, InstructionType.I),
          (Opcodes64.SRAI, ShiftOp.SRA, InstructionType.I)
        )
      } else {
        Seq(
          (Opcodes.SLLI, ShiftOp.SLL, InstructionType.I),
          (Opcodes.SRLI, ShiftOp.SRL, InstructionType.I),
          (Opcodes.SRAI, ShiftOp.SRA, InstructionType.I)
        )
      }

      val reg_ops = Seq(
        (Opcodes.SLL, ShiftOp.SLL, InstructionType.R),
        (Opcodes.SRL, ShiftOp.SRL, InstructionType.R),
        (Opcodes.SRA, ShiftOp.SRA, InstructionType.R)
      )

      for ((opcode, op, itype) <- reg_ops ++ imm_ops) {
        dconfig.addDecoding(
          opcode,
          itype,
          Map(
            Data.SHIFT_OP -> op
          )
        )
        issuer.setDestinations(opcode, exeStages)
      }

      if (config.xlen == 64) {
        // add 64-bit specific operations

        val ops = Seq(
          (Opcodes64.SLLIW, ShiftOp.SLL, InstructionType.I),
          (Opcodes64.SRLIW, ShiftOp.SRL, InstructionType.I),
          (Opcodes64.SRAIW, ShiftOp.SRA, InstructionType.I),
          (Opcodes64.SLLW, ShiftOp.SLL, InstructionType.R),
          (Opcodes64.SRLW, ShiftOp.SRL, InstructionType.R),
          (Opcodes64.SRAW, ShiftOp.SRA, InstructionType.R)
        )

        for ((opcode, op, itype) <- ops) {
          dconfig.addDecoding(
            opcode,
            itype,
            Map(
              Data.SHIFT_OP -> op,
              Data.SHIFT_TRUNC -> True
            )
          )
          issuer.setDestinations(opcode, exeStages)
        }
      }
    }
  }

  override def build(): Unit = {
    for (exeStage <- exeStages) {
      exeStage plug new Area {
        import exeStage._

        val src = UInt(config.xlen bits)
        val useImm = value(pipeline.data.IMM_USED)
        val imm_len = if (config.xlen == 64) 6 else 5
        val shamt = UInt(imm_len bits)

        val op = value(Data.SHIFT_OP)

        if (config.xlen == 64) {
          when(input(Data.SHIFT_TRUNC)) {
            when(op === ShiftOp.SRA) {
              src := Utils.signExtend(value(pipeline.data.RS1_DATA)(31 downto 0), 64)
            } otherwise {
              src := value(pipeline.data.RS1_DATA)(31 downto 0).resized
            }
          } otherwise {
            src := value(pipeline.data.RS1_DATA)
          }
        } else {
          src := value(pipeline.data.RS1_DATA)
        }

        when(useImm) {
          if (config.xlen == 64) {
            shamt := value(pipeline.data.IMM)(5 downto 0)
          } else {
            shamt := value(pipeline.data.IMM)(4 downto 0)
          }
        } otherwise {
          if (config.xlen == 64) {
            when(input(Data.SHIFT_TRUNC)) {
              shamt := value(pipeline.data.RS2_DATA)(4 downto 0).resized
            } otherwise {
              shamt := value(pipeline.data.RS2_DATA)(5 downto 0)
            }
          } else {
            shamt := value(pipeline.data.RS2_DATA)(4 downto 0)
          }
        }

        val result = op.mux(
          ShiftOp.NONE -> U(0),
          ShiftOp.SLL -> (src |<< shamt),
          ShiftOp.SRL -> (src |>> shamt),
          ShiftOp.SRA -> (src.asSInt >> shamt).asUInt
        )

        when(arbitration.isValid && op =/= ShiftOp.NONE) {
          arbitration.rs1Needed := True
          arbitration.rs2Needed := !useImm
          if (config.xlen == 64) {
            when(input(Data.SHIFT_TRUNC)) {
              output(pipeline.data.RD_DATA) := Utils.signExtend(result(31 downto 0), 64)
            } otherwise {
              output(pipeline.data.RD_DATA) := result
            }
          } else {
            output(pipeline.data.RD_DATA) := result
          }
          output(pipeline.data.RD_DATA_VALID) := True
        }
      }
    }
  }
}
