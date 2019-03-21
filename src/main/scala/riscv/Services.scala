package riscv

import riscv._

import spinal.core._

trait DecoderService {
  type Action = Map[PipelineData[_ <: Data], Data]

  trait DecoderConfig {
    def addDecoding(key: MaskedLiteral, action: Action): Unit
    def addDefault(action: Action): Unit
    def addDefault(data: PipelineData[_ <: Data], value: Data): Unit
  }

  protected val config: DecoderConfig
  protected def stage(pipeline: Pipeline): Stage

  def configure(pipeline: Pipeline)(f: DecoderConfig => Unit): Unit = {
    stage(pipeline).rework(f(config))
  }
}
