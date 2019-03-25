package riscv

sealed trait InstructionType

object InstructionType {
  case object R extends InstructionType
  case object I extends InstructionType
  case object S extends InstructionType
  case object B extends InstructionType
  case object U extends InstructionType
  case object J extends InstructionType
}
