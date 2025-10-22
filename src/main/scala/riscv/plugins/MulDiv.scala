package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

private object Data {
  object MUL extends PipelineData(Bool())
  object MUL_HIGH extends PipelineData(Bool())
  object MULDIV_RS1_SIGNED extends PipelineData(Bool())
  object MULDIV_RS2_SIGNED extends PipelineData(Bool())
  object DIV extends PipelineData(Bool())
  object REM extends PipelineData(Bool())
  object TRUNC extends PipelineData(Bool())
}

class MulDiv(exeStages: Set[Stage]) extends Plugin[Pipeline] {

  override def getImplementedExtensions = Seq('M')

  override def setup(): Unit = {
    val issueService = pipeline.service[IssueService]

    pipeline.service[DecoderService].configure { decoder =>
      decoder.addDefault(
        Map(
          Data.MUL -> False,
          Data.MUL_HIGH -> False,
          Data.DIV -> False,
          Data.REM -> False,
          Data.MULDIV_RS1_SIGNED -> False,
          Data.MULDIV_RS2_SIGNED -> False,
          Data.TRUNC -> False
        )
      )

      val mulDecodings = Seq(
        (Opcodes.MUL, False, True, True),
        (Opcodes.MULH, True, True, True),
        (Opcodes.MULHSU, True, True, False),
        (Opcodes.MULHU, True, False, False)
      )

      for ((opcode, high, rs1Signed, rs2Signed) <- mulDecodings) {
        decoder.addDecoding(
          opcode,
          InstructionType.R,
          Map(
            Data.MUL -> True,
            Data.MUL_HIGH -> high,
            Data.MULDIV_RS1_SIGNED -> rs1Signed,
            Data.MULDIV_RS2_SIGNED -> rs2Signed
          )
        )

        issueService.setDestinations(opcode, exeStages)
      }

      val divDecodings = Seq(
        (Opcodes.DIV, False, True),
        (Opcodes.DIVU, False, False),
        (Opcodes.REM, True, True),
        (Opcodes.REMU, True, False)
      )

      for ((opcode, rem, signed) <- divDecodings) {
        decoder.addDecoding(
          opcode,
          InstructionType.R,
          Map(
            Data.DIV -> True,
            Data.REM -> rem,
            Data.MULDIV_RS1_SIGNED -> signed,
            Data.MULDIV_RS2_SIGNED -> signed
          )
        )

        issueService.setDestinations(opcode, exeStages)
      }

      if (config.xlen == 64) {
        val mulDecodings = Seq(
          (Opcodes64.MULW, False, True, True)
        )

        for ((opcode, high, rs1Signed, rs2Signed) <- mulDecodings) {
          decoder.addDecoding(
            opcode,
            InstructionType.R,
            Map(
              Data.MUL -> True,
              Data.MUL_HIGH -> high,
              Data.MULDIV_RS1_SIGNED -> rs1Signed,
              Data.MULDIV_RS2_SIGNED -> rs2Signed,
              Data.TRUNC -> True
            )
          )

          issueService.setDestinations(opcode, exeStages)
        }

        val divDecodings = Seq(
          (Opcodes64.DIVW, False, True),
          (Opcodes64.DIVUW, False, False),
          (Opcodes64.REMW, True, True),
          (Opcodes64.REMUW, True, False)
        )

        for ((opcode, rem, signed) <- divDecodings) {
          decoder.addDecoding(
            opcode,
            InstructionType.R,
            Map(
              Data.DIV -> True,
              Data.REM -> rem,
              Data.MULDIV_RS1_SIGNED -> signed,
              Data.MULDIV_RS2_SIGNED -> signed,
              Data.TRUNC -> True
            )
          )

          issueService.setDestinations(opcode, exeStages)
        }
      }
    }
  }

  override def build(): Unit = {

    for (mulStage <- exeStages) {
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
        val step = Counter(config.xlen + 1)
        val multiplicand = UInt(config.xlen + 1 bits)
        val multiplier = UInt(config.xlen bits)
        multiplicand := 0
        multiplier := 0
        val isMul = value(Data.MUL)

        when(arbitration.isIdle) {
          initMul := True
        }

        when(arbitration.isValid && isMul) {
          arbitration.rs1Needed := True
          arbitration.rs2Needed := True
        }

        when(arbitration.isRunning && isMul) {
          arbitration.isReady := False

          val rs1 = value(pipeline.data.RS1_DATA)
          val rs2 = value(pipeline.data.RS2_DATA)

          when(value(Data.MULDIV_RS2_SIGNED) && rs2.asSInt < 0) {
            // If RS2 is signed, RS1 is also signed
            multiplicand :=
              Utils.signExtend(Utils.twosComplement(rs1), config.xlen + 1)
            multiplier := Utils.twosComplement(rs2)
          } otherwise {
            when(value(Data.MULDIV_RS1_SIGNED)) {
              multiplicand := Utils.signExtend(rs1, config.xlen + 1)
            } otherwise {
              multiplicand := Utils.zeroExtend(rs1, config.xlen + 1)
            }

            multiplier := rs2
          }

          when(initMul) {
            productH := 0
            productL := multiplier
            step.clear()
            initMul := False
          } elsewhen (step.willOverflowIfInc) {
            step.increment()
            initMul := True

            val result = value(Data.MUL_HIGH) ? productH | productL

            if (config.xlen == 64) {
              when(input(Data.TRUNC)) {
                output(pipeline.data.RD_DATA) := Utils.signExtend(result(31 downto 0), 64)
              } otherwise {
                output(pipeline.data.RD_DATA) := result
              }
            } else {
              output(pipeline.data.RD_DATA) := result
            }

            arbitration.isReady := True
            output(pipeline.data.RD_DATA_VALID) := True
          } otherwise {
            step.increment()

            val extendedProductH = UInt(config.xlen + 1 bits)

            when(value(Data.MULDIV_RS1_SIGNED) || value(Data.MULDIV_RS2_SIGNED)) {
              extendedProductH := Utils.signExtend(productH, config.xlen + 1)
            } otherwise {
              extendedProductH := Utils.zeroExtend(productH, config.xlen + 1)
            }

            val partialSum = UInt(config.xlen + 1 bits)

            when(product.lsb) {
              when(
                value(Data.MUL_HIGH) && rs1.asSInt < 0 && rs2.asSInt < 0 &&
                  value(Data.MULDIV_RS2_SIGNED) && value(Data.MULDIV_RS1_SIGNED)
              ) {
                partialSum := extendedProductH + multiplicand(config.xlen - 1 downto 0)
              } elsewhen (step.value === config.xlen - 1 && rs1.asSInt < 0
                && value(Data.MULDIV_RS2_SIGNED) && value(Data.MULDIV_RS1_SIGNED)) {
                partialSum := extendedProductH + multiplicand(config.xlen - 1 downto 0)
              } otherwise {
                partialSum := extendedProductH + multiplicand
              }
            } otherwise {
              partialSum := extendedProductH
            }

            product := partialSum @@ product(config.xlen - 1 downto 1)
          }
        }
      }
    }

    for (divStage <- exeStages) {
      divStage plug new Area {
        import divStage._

        val quotient = Reg(UInt(config.xlen bits)).init(0)
        val remainder = Reg(UInt(config.xlen bits)).init(0)
        val initDiv = Reg(Bool()).init(True)
        val step = Counter(config.xlen + 1)
        val isDiv = value(Data.DIV)

        when(arbitration.isIdle) {
          initDiv := True
        }

        when(arbitration.isValid && isDiv) {
          arbitration.rs1Needed := True
          arbitration.rs2Needed := True
        }

        when(arbitration.isRunning && isDiv) {
          arbitration.isReady := False

          val rs1 = value(pipeline.data.RS1_DATA)
          val rs2 = value(pipeline.data.RS2_DATA)
          val signed = value(Data.MULDIV_RS1_SIGNED)
          val dividendIsNeg = signed && (rs1.asSInt < 0)
          val divisorIsNeg = signed && (rs2.asSInt < 0)
          val dividend = dividendIsNeg ? Utils.twosComplement(rs1) | rs1
          val divisor = divisorIsNeg ? Utils.twosComplement(rs2) | rs2

          when(divisor === 0) {
            arbitration.isReady := True
            output(pipeline.data.RD_DATA) :=
              value(Data.REM) ? rs1 | S(-1, config.xlen bits).asUInt
            output(pipeline.data.RD_DATA_VALID) := True
          } elsewhen (initDiv) {
            quotient := dividend
            remainder := 0
            step.clear()
            initDiv := False
          } elsewhen (step.willOverflowIfInc) {
            val correctedQuotient, correctedRemainder = UInt(config.xlen bits)
            correctedQuotient := quotient
            correctedRemainder := remainder

            when(dividendIsNeg =/= divisorIsNeg) {
              correctedQuotient := Utils.twosComplement(quotient)
            }

            when(dividendIsNeg) {
              correctedRemainder := Utils.twosComplement(remainder)
            }

            val result = value(Data.REM) ? correctedRemainder | correctedQuotient

            if (config.xlen == 64) {
              when(input(Data.TRUNC)) {
                output(pipeline.data.RD_DATA) := Utils.signExtend(result(31 downto 0), 64)
              } otherwise {
                output(pipeline.data.RD_DATA) := result
              }
            } else {
              output(pipeline.data.RD_DATA) := result
            }

            output(pipeline.data.RD_DATA_VALID) := True
            arbitration.isReady := True
            step.increment()
            initDiv := True
          } otherwise {
            val newRemainder = remainder(config.xlen - 2 downto 0) @@ quotient.msb
            val quotientLsb = Bool()

            when(newRemainder >= divisor) {
              remainder := newRemainder - divisor
              quotientLsb := True
            } otherwise {
              remainder := newRemainder
              quotientLsb := False
            }

            quotient := quotient(config.xlen - 2 downto 0) @@ quotientLsb

            step.increment()
          }
        }
      }
    }
  }
}
