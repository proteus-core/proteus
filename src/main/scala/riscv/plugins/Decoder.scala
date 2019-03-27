package riscv.plugins

import riscv._

import spinal.core._
import spinal.core.internals.Literal

import scala.collection.mutable

case class ImmediateDecoder(ir: Bits) {
  private def signExtend(data: Bits): UInt = {
    DataTools.signExtend(data, 32).asUInt
  }

  def i = signExtend(ir(31 downto 20))
  def s = signExtend(ir(31 downto 25) ## ir(11 downto 7))
  def b = signExtend(ir(31) ## ir(7) ## ir(30 downto 25) ## ir(11 downto 8) ## False)
  def u = (ir(31 downto 12) << 12).asUInt
  def j = signExtend(ir(31) ## ir(19 downto 12) ## ir(20) ## ir(30 downto 25) ## ir(24 downto 21) ## False)
}

class Decoder extends Plugin with DecoderService {
  private val instructionTypes = mutable.Map[MaskedLiteral, InstructionType]()
  private val decodings = mutable.Map[MaskedLiteral, Action]()
  private val defaults = mutable.Map[PipelineData[_ <: Data], Data]()

  override protected val config = new DecoderConfig {
    override def addDecoding(opcode: MaskedLiteral,
                             itype: InstructionType,
                             action: Action): Unit = {
      assert(!instructionTypes.contains(opcode),
        s"Multiple instruction types set for $opcode")

      instructionTypes(opcode) = itype
      addDecoding(opcode, action)
    }

    override def addDecoding(opcode: MaskedLiteral, action: Action): Unit = {
      val currentAction = decodings.getOrElse(opcode, Map())

      for ((key, data) <- currentAction) {
        assert(!action.contains(key),
          s"Conflicting decodings for opcode $opcode: $key overspecified")
      }

      decodings(opcode) = currentAction ++ action
    }

    override def addDefault(action: Action): Unit = {
      defaults ++= action
    }

    override def addDefault(data: PipelineData[_ <: Data], value: Data): Unit = {
      defaults(data) = value
    }
  }

  override def setup(pipeline: Pipeline, config: Config): Unit = {
    configure(pipeline) {config =>
      config.addDefault(Map(
        pipeline.data.UNKNOWN_INSTRUCTION -> False,
        pipeline.data.RD_VALID -> False,
        pipeline.data.IMM -> U(0)
      ))
    }
  }

  override protected def stage(pipeline: Pipeline): Stage = pipeline.decode

  override def build(pipeline: Pipeline, config: Config): Unit = {
    pipeline.decode plug new Area {
      import pipeline.decode._

      val ir = value(pipeline.data.IR)
      output(pipeline.data.RS1) := ir(19 downto 15)
      output(pipeline.data.RS2) := ir(24 downto 20)
      output(pipeline.data.RD) := ir(11 downto 7)

      applyAction(pipeline.decode, defaults.toMap)

      val immDecoder = ImmediateDecoder(ir.asBits)

      switch (value(pipeline.data.IR)) {
        for ((key, action) <- decodings) {
          is (key) {
            applyAction(pipeline.decode, action)

            assert(instructionTypes.contains(key),
              s"Opcode $key has decodings but no instruction type set")

            val instructionType = instructionTypes(key)

            val imm = instructionType match {
              case InstructionType.I => immDecoder.i
              case InstructionType.S => immDecoder.s
              case InstructionType.B => immDecoder.b
              case InstructionType.U => immDecoder.u
              case InstructionType.J => immDecoder.j
              case InstructionType.R => U(0)
            }

            output(pipeline.data.IMM) := imm

            if (!instructionType.rs1Used) {
              output(pipeline.data.RS1) := U(0)
            }
            if (!instructionType.rs2Used) {
              output(pipeline.data.RS2) := U(0)
            }
            if (!instructionType.rdUsed) {
              output(pipeline.data.RD) := U(0)
            }
          }
        }
        default {
          output(pipeline.data.UNKNOWN_INSTRUCTION) := True
        }
      }
    }
  }

  private def applyAction(stage: Stage, action: Action) = {
    for ((data, value) <- action) {
      val out = stage.output(data)
      out := value
    }
  }
}
