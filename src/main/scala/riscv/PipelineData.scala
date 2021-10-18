package riscv

import spinal.core._

class PipelineData[T <: Data](val dataType: HardType[T]) {
  def name: String = getClass.getSimpleName.takeWhile(_ != '$')

  override def toString: String = name
}

class StandardPipelineData(config: Config) {
  private val xlen = config.xlen

  object PC extends PipelineData(UInt(xlen bits))
  object NEXT_PC extends PipelineData(UInt(xlen bits))
  object IR extends PipelineData(UInt(32 bits))
  object RS1 extends PipelineData(UInt(5 bits))
  object RS1_DATA extends PipelineData(UInt(xlen bits))
  object RS1_TYPE extends PipelineData(RegisterType())
  object RS2 extends PipelineData(UInt(5 bits))
  object RS2_DATA extends PipelineData(UInt(xlen bits))
  object RS2_TYPE extends PipelineData(RegisterType())
  object RD extends PipelineData(UInt(5 bits))
  object RD_DATA extends PipelineData(UInt(xlen bits))
  object RD_TYPE extends PipelineData(RegisterType())
  object RD_VALID extends PipelineData(Bool())
  object IMM extends PipelineData(UInt(32 bits))
  object IMM_USED extends PipelineData(Bool())
}
