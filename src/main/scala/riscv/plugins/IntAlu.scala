package riscv.plugins

import riscv._

import spinal.core._

class IntAlu extends Plugin {
  object Opcodes {
    val ADD  = M"0000000----------000-----0110011"
    val SUB  = M"0100000----------000-----0110011"
    val SLT  = M"0000000----------010-----0110011"
    val SLTU = M"0000000----------011-----0110011"
    val XOR  = M"0000000----------100-----0110011"
    val OR   = M"0000000----------110-----0110011"
    val AND  = M"0000000----------111-----0110011"
    val ADDI = M"-----------------000-----0010011"
  }

  object AluOp extends SpinalEnum {
    val ADD, SUB, SLT, SLTU, XOR, OR, AND = newElement()
  }

  object Data {
    object ALU_OP extends PipelineData(AluOp())
    object ALU_SRC2_IMM extends PipelineData(Bool())
  }

  override def setup(pipeline: Pipeline, config: Config): Unit = {
    val decoder = pipeline.getService[DecoderService]

    decoder.configure(pipeline) {config =>
      config.addDefault(Map(
        Data.ALU_SRC2_IMM -> False
      ))

      config.addDecoding(Opcodes.ADD, Map(
        Data.ALU_OP -> AluOp.ADD,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.SUB, Map(
        Data.ALU_OP -> AluOp.SUB,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.SLT, Map(
        Data.ALU_OP -> AluOp.SLT,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.SLTU, Map(
        Data.ALU_OP -> AluOp.SLTU,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.XOR, Map(
        Data.ALU_OP -> AluOp.XOR,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.OR, Map(
        Data.ALU_OP -> AluOp.OR,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.AND, Map(
        Data.ALU_OP -> AluOp.AND,
        pipeline.data.WRITE_RD -> True
      ))

      config.addDecoding(Opcodes.ADDI, Map(
        Data.ALU_OP -> AluOp.ADD,
        Data.ALU_SRC2_IMM -> True,
        pipeline.data.WRITE_RD -> True
      ))
    }
  }

  override def build(pipeline: Pipeline, config: Config): Unit = {
    pipeline.execute plug new Area {
      import pipeline.execute._

      val op = value(Data.ALU_OP)
      val src1 = value(pipeline.data.RS1_DATA)
      val src2 = value(Data.ALU_SRC2_IMM) ? value(pipeline.data.IMM) | value(pipeline.data.RS2_DATA)
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
      }

      output(pipeline.data.RD_DATA) := result
    }
  }
}
