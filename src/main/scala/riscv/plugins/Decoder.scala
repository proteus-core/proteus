package riscv.plugins

import riscv._

import spinal.core._
import spinal.core.internals.Literal

import scala.collection.mutable

class Decoder extends Plugin with DecoderService {
  private val decodings = mutable.Map[MaskedLiteral, Action]()
  private val defaults = mutable.Map[PipelineData[_ <: Data], Data]()

  override protected val config = new DecoderConfig {
    override def addDecoding(key: MaskedLiteral, action: Action): Unit = {
      assert(!decodings.contains(key), s"Multiple decodings for $key")
      decodings(key) = action
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
      config.addDefault(pipeline.data.UNKNOWN_INSTRUCTION, False)
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

      val imm_i = U((31 downto 12) -> ir(31), (11 downto 0) -> ir(31 downto 20))
      output(pipeline.data.IMM) := imm_i

      applyAction(pipeline.decode, defaults.toMap)

      switch (value(pipeline.data.IR)) {
        for ((key, action) <- decodings) {
          is (key) {
            applyAction(pipeline.decode, action)
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
