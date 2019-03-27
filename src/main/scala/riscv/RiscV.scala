package riscv

sealed abstract class InstructionType(val rs1Used: Boolean,
                                      val rs2Used: Boolean,
                                      val rdUsed: Boolean)

object InstructionType {
  case object R extends InstructionType(true,  true,  true)
  case object I extends InstructionType(true,  false, true)
  case object S extends InstructionType(true,  true,  false)
  case object B extends InstructionType(true,  true,  false)
  case object U extends InstructionType(false, false, true)
  case object J extends InstructionType(false, false, true)
}
