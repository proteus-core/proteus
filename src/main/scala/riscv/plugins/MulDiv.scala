package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

private object Opcodes {
  val MUL    = M"0000001----------000-----0110011"
  val MULH   = M"0000001----------001-----0110011"
  val MULHSU = M"0000001----------010-----0110011"
  val MULHU  = M"0000001----------011-----0110011"
  val DIV    = M"0000001----------100-----0110011"
  val DIVU   = M"0000001----------101-----0110011"
  val REM    = M"0000001----------110-----0110011"
  val REMU   = M"0000001----------111-----0110011"
}

private object Data {
  object MUL extends PipelineData(Bool())
  object MUL_HIGH extends PipelineData(Bool())
  object MUL_RS1_SIGNED extends PipelineData(Bool())
  object MUL_RS2_SIGNED extends PipelineData(Bool())
}

class MulDiv(implicit config: Config) extends Plugin {
  override def setup(pipeline: Pipeline): Unit = {
    pipeline.getService[DecoderService].configure(pipeline) {decoder =>
      decoder.addDefault(Map(
        Data.MUL -> False,
        Data.MUL_HIGH -> False,
        Data.MUL_RS1_SIGNED -> False,
        Data.MUL_RS2_SIGNED -> False
      ))

      val decodings = Seq(
        (Opcodes.MUL,    False, True,  True),
        (Opcodes.MULH,   True,  True,  True),
        (Opcodes.MULHSU, True,  True,  False),
        (Opcodes.MULHU,  True,  False, False)
      )

      for ((opcode, high, rs1Signed, rs2Signed) <- decodings) {
        decoder.addDecoding(opcode, InstructionType.R, Map(
          Data.MUL -> True,
          Data.MUL_HIGH -> high,
          Data.MUL_RS1_SIGNED -> rs1Signed,
          Data.MUL_RS2_SIGNED -> rs2Signed,
          pipeline.data.WRITE_RD -> True
        ))
      }
    }
  }

  override def build(pipeline: Pipeline): Unit = {
    val mulStage = pipeline.execute

    mulStage plug new Area {
      import mulStage._

      // Multiplication implementation is based on algorithm version 3 from
      // https://www.slideshare.net/TanjarulIslamMishu/multiplication-algorithm-hardware-and-flowchart

      // These variables are declared here to make sure they will be added to
      // the waveform
      val product = Reg(UInt(config.xlen * 2 bits))
      val productH = product(config.xlen * 2 - 1 downto config.xlen)
      val productL = product(config.xlen - 1 downto 0)
      val initMul = Reg(Bool()).init(True)
      val step = Counter(33)
      val multiplicand = UInt(config.xlen + 1 bits)
      val multiplier = UInt(config.xlen bits)
      multiplicand := 0
      multiplier := 0

      when (arbitration.isValid && value(Data.MUL)) {
        arbitration.isReady := False
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        val rs1 = value(pipeline.data.RS1_DATA)
        val rs2 = value(pipeline.data.RS2_DATA)

        when (value(Data.MUL_RS2_SIGNED) && rs2.asSInt < 0) {
          // If RS2 is signed, RS1 is also signed
          multiplicand := DataTools.signExtend(0 - rs1, config.xlen + 1)
          multiplier := 0 - rs2
        } otherwise {
          when (value(Data.MUL_RS1_SIGNED)) {
            multiplicand := DataTools.signExtend(rs1, config.xlen + 1)
          } otherwise {
            multiplicand := DataTools.zeroExtend(rs1, config.xlen + 1)
          }

          multiplier := rs2
        }

        when (initMul) {
          productH := 0
          productL := multiplier
          initMul := False
        } elsewhen (step.willOverflowIfInc) {
          step.increment()
          initMul := True

          arbitration.isReady := True
          output(pipeline.data.RD_DATA) := value(Data.MUL_HIGH) ? productH | productL
          output(pipeline.data.RD_VALID) := True
        } otherwise {
          step.increment()

          val extendedProductH = UInt(config.xlen + 1 bits)

          when (value(Data.MUL_RS1_SIGNED) || value(Data.MUL_RS2_SIGNED)) {
            extendedProductH := DataTools.signExtend(productH, config.xlen + 1)
          } otherwise {
            extendedProductH := DataTools.zeroExtend(productH, config.xlen + 1)
          }

          val partialSum = UInt(config.xlen + 1 bits)

          when (product.lsb) {
            partialSum := extendedProductH + multiplicand
          } otherwise {
            partialSum := extendedProductH
          }

          product := partialSum @@ product(config.xlen - 1 downto 1)
        }
      }
    }
  }
}
