package riscv

import riscv._

import spinal.core._

trait IBusService {
  def getIBus: MemBus
}

trait DBusService {
  def getDBus: MemBus
}

trait DecoderService {
  type Action = Map[PipelineData[_ <: Data], Data]

  trait DecoderConfig {
    def addDecoding(opcode: MaskedLiteral,
                    itype: InstructionType,
                    action: Action): Unit
    def addDecoding(opcode: MaskedLiteral, action: Action): Unit
    def addDefault(action: Action): Unit
    def addDefault(data: PipelineData[_ <: Data], value: Data): Unit
  }

  protected val config: DecoderConfig
  protected def stage(pipeline: Pipeline): Stage

  def configure(pipeline: Pipeline)(f: DecoderConfig => Unit): Unit = {
    stage(pipeline).rework(f(config))
  }
}

trait IntAluService {
  object AluOp extends SpinalEnum {
    val ADD, SUB, SLT, SLTU, XOR, OR, AND, SRC2 = newElement()
  }

  object Src1Select extends SpinalEnum {
    val RS1, PC = newElement()
  }

  object Src2Select extends SpinalEnum {
    val RS2, IMM = newElement()
  }

  def addOperation(pipeline: Pipeline, opcode: MaskedLiteral,
                   op: SpinalEnumElement[AluOp.type],
                   src1: SpinalEnumElement[Src1Select.type],
                   src2: SpinalEnumElement[Src2Select.type]): Unit
}
