package riscv.plugins

import riscv._

import spinal.core._

class IntAlu(aluStages: Set[Stage]) extends Plugin[Pipeline] with IntAluService {
  object Data {
    object ALU_OP extends PipelineData(AluOp())
    object ALU_SRC1 extends PipelineData(Src1Select())
    object ALU_SRC2 extends PipelineData(Src2Select())
    object ALU_COMMIT_RESULT extends PipelineData(Bool())
    object ALU_RESULT extends PipelineData(UInt(config.xlen bits))
  }

  override def addOperation(opcode: MaskedLiteral,
                            op: SpinalEnumElement[AluOp.type],
                            src1: SpinalEnumElement[Src1Select.type],
                            src2: SpinalEnumElement[Src2Select.type]): Unit = {
    pipeline.getService[DecoderService].configure {config =>
      config.addDecoding(opcode, Map(
        Data.ALU_OP -> op,
        Data.ALU_SRC1 -> src1,
        Data.ALU_SRC2 -> src2
      ))
    }
  }

  override def resultData: PipelineData[UInt] = Data.ALU_RESULT

  override def setup(): Unit = {
    val decoder = pipeline.getService[DecoderService]
    val issuer = pipeline.getService[IssueService]

    decoder.configure {config =>
      config.addDefault(Map(
        Data.ALU_SRC1 -> Src1Select.RS1,
        Data.ALU_COMMIT_RESULT -> False
      ))

      val regRegOpcodes = Map(
        Opcodes.ADD  -> AluOp.ADD,
        Opcodes.SUB  -> AluOp.SUB,
        Opcodes.SLT  -> AluOp.SLT,
        Opcodes.SLTU -> AluOp.SLTU,
        Opcodes.XOR  -> AluOp.XOR,
        Opcodes.OR   -> AluOp.OR,
        Opcodes.AND  -> AluOp.AND
      )

      for ((opcode, op) <- regRegOpcodes) {
        config.addDecoding(opcode, InstructionType.R, Map(
          Data.ALU_OP -> op,
          Data.ALU_SRC2 -> Src2Select.RS2,
          Data.ALU_COMMIT_RESULT -> True
        ))

        issuer.setDestinations(opcode, aluStages)
      }

      val regImmOpcodes = Map(
        Opcodes.ADDI  -> AluOp.ADD,
        Opcodes.SLTI  -> AluOp.SLT,
        Opcodes.SLTIU -> AluOp.SLTU,
        Opcodes.XORI  -> AluOp.XOR,
        Opcodes.ORI   -> AluOp.OR,
        Opcodes.ANDI  -> AluOp.AND
      )

      for ((opcode, op) <- regImmOpcodes) {
        config.addDecoding(opcode, InstructionType.I, Map(
          Data.ALU_OP -> op,
          Data.ALU_SRC2 -> Src2Select.IMM,
          Data.ALU_COMMIT_RESULT -> True
        ))

        issuer.setDestinations(opcode, aluStages)
      }

      config.addDecoding(Opcodes.LUI, InstructionType.U, Map(
        Data.ALU_OP -> AluOp.SRC2,
        Data.ALU_SRC2 -> Src2Select.IMM,
        Data.ALU_COMMIT_RESULT -> True
      ))

      issuer.setDestinations(Opcodes.LUI, aluStages)

      config.addDecoding(Opcodes.AUIPC, InstructionType.U, Map(
        Data.ALU_OP -> AluOp.ADD,
        Data.ALU_SRC1 -> Src1Select.PC,
        Data.ALU_SRC2 -> Src2Select.IMM,
        Data.ALU_COMMIT_RESULT -> True
      ))

      issuer.setDestinations(Opcodes.AUIPC, aluStages)
    }
  }

  override def build(): Unit = {
    for (stage <- aluStages) {
      stage plug new Area {
        import stage._

        val op = value(Data.ALU_OP)
        val src1, src2 = UInt(config.xlen bits)

        switch (value(Data.ALU_SRC1)) {
          is (Src1Select.RS1) {
            src1 := value(pipeline.data.RS1_DATA)
            arbitration.rs1Needed := True
          }
          is (Src1Select.PC) {
            src1 := value(pipeline.data.PC)
          }
        }

        switch (value(Data.ALU_SRC2)) {
          is (Src2Select.RS2) {
            src2 := value(pipeline.data.RS2_DATA)
            arbitration.rs2Needed := True
          }
          is (Src2Select.IMM) {
            src2 := value(pipeline.data.IMM)
          }
        }

        val result = UInt(config.xlen bits)

        switch (op) {
          is (AluOp.ADD) {
            result := src1 + src2
          }
          is (AluOp.SUB) {
            result := src1 - src2
          }
          is (AluOp.SLT) {
            result := (src1.asSInt < src2.asSInt).asUInt.resized
          }
          is (AluOp.SLTU) {
            result := (src1 < src2).asUInt.resized
          }
          is (AluOp.XOR) {
            result := src1 ^ src2
          }
          is (AluOp.OR) {
            result := src1 | src2
          }
          is (AluOp.AND) {
            result := src1 & src2
          }
          is (AluOp.SRC2) {
            result := src2
          }
        }

        when (value(Data.ALU_COMMIT_RESULT)) {
          output(pipeline.data.RD_DATA) := result
          output(pipeline.data.RD_DATA_VALID) := True
        } otherwise {
          output(Data.ALU_RESULT) := result
        }
      }
    }
  }
}
