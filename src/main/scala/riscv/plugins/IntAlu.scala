package riscv.plugins

import riscv._

import spinal.core._

class IntAlu extends Plugin with IntAluService {
  object Opcodes {
    val ADD   = M"0000000----------000-----0110011"
    val SUB   = M"0100000----------000-----0110011"
    val SLT   = M"0000000----------010-----0110011"
    val SLTU  = M"0000000----------011-----0110011"
    val XOR   = M"0000000----------100-----0110011"
    val OR    = M"0000000----------110-----0110011"
    val AND   = M"0000000----------111-----0110011"
    val ADDI  = M"-----------------000-----0010011"
    val SLTI  = M"-----------------010-----0010011"
    val SLTIU = M"-----------------011-----0010011"
    val XORI  = M"-----------------100-----0010011"
    val ORI   = M"-----------------110-----0010011"
    val ANDI  = M"-----------------111-----0010011"
    val LUI   = M"-------------------------0110111"
  }

  object Data {
    object ALU_OP extends PipelineData(AluOp())
    object ALU_SRC1 extends PipelineData(Src1Select())
    object ALU_SRC2 extends PipelineData(Src2Select())
    object ALU_COMMIT_RESULT extends PipelineData(Bool())
  }

  override def addOperation(pipeline: Pipeline,
                            opcode: MaskedLiteral,
                            op: SpinalEnumElement[AluOp.type],
                            src1: SpinalEnumElement[Src1Select.type],
                            src2: SpinalEnumElement[Src2Select.type]): Unit = {
    pipeline.getService[DecoderService].configure(pipeline) {config =>
      config.addDecoding(opcode, Map(
        Data.ALU_OP -> op,
        Data.ALU_SRC1 -> src1,
        Data.ALU_SRC2 -> src2
      ))
    }
  }

  override def setup(pipeline: Pipeline, config: Config): Unit = {
    val decoder = pipeline.getService[DecoderService]

    decoder.configure(pipeline) {config =>
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
          Data.ALU_COMMIT_RESULT -> True,
          pipeline.data.WRITE_RD -> True
        ))
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
          Data.ALU_COMMIT_RESULT -> True,
          pipeline.data.WRITE_RD -> True
        ))
      }

      config.addDecoding(Opcodes.LUI, InstructionType.U, Map(
        Data.ALU_OP -> AluOp.SRC2,
        Data.ALU_SRC2 -> Src2Select.IMM,
        Data.ALU_COMMIT_RESULT -> True,
        pipeline.data.WRITE_RD -> True
      ))
    }
  }

  override def build(pipeline: Pipeline, config: Config): Unit = {
    pipeline.execute plug new Area {
      import pipeline.execute._

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

      output(pipeline.data.RD_DATA) := result

      when (value(Data.ALU_COMMIT_RESULT)) {
        output(pipeline.data.RD_VALID) := True
      }
    }
  }
}
