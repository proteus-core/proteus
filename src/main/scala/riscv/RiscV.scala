package riscv

import spinal.core._

object Opcodes {
  val ADD    = M"0000000----------000-----0110011"
  val SUB    = M"0100000----------000-----0110011"
  val SLT    = M"0000000----------010-----0110011"
  val SLTU   = M"0000000----------011-----0110011"
  val XOR    = M"0000000----------100-----0110011"
  val OR     = M"0000000----------110-----0110011"
  val AND    = M"0000000----------111-----0110011"
  val ADDI   = M"-----------------000-----0010011"
  val SLTI   = M"-----------------010-----0010011"
  val SLTIU  = M"-----------------011-----0010011"
  val XORI   = M"-----------------100-----0010011"
  val ORI    = M"-----------------110-----0010011"
  val ANDI   = M"-----------------111-----0010011"
  val LUI    = M"-------------------------0110111"
  val AUIPC  = M"-------------------------0010111"

  val SLLI   = M"0000000----------001-----0010011"
  val SRLI   = M"0000000----------101-----0010011"
  val SRAI   = M"0100000----------101-----0010011"
  val SLL    = M"0000000----------001-----0110011"
  val SRL    = M"0000000----------101-----0110011"
  val SRA    = M"0100000----------101-----0110011"

  val JAL    = M"-------------------------1101111"
  val JALR   = M"-----------------000-----1100111"
  val BEQ    = M"-----------------000-----1100011"
  val BNE    = M"-----------------001-----1100011"
  val BLT    = M"-----------------100-----1100011"
  val BGE    = M"-----------------101-----1100011"
  val BLTU   = M"-----------------110-----1100011"
  val BGEU   = M"-----------------111-----1100011"

  val LB     = M"-----------------000-----0000011"
  val LH     = M"-----------------001-----0000011"
  val LW     = M"-----------------010-----0000011"
  val LBU    = M"-----------------100-----0000011"
  val LHU    = M"-----------------101-----0000011"
  val SB     = M"-----------------000-----0100011"
  val SH     = M"-----------------001-----0100011"
  val SW     = M"-----------------010-----0100011"

  val CSRRW  = M"-----------------001-----1110011"
  val CSRRS  = M"-----------------010-----1110011"
  val CSRRC  = M"-----------------011-----1110011"
  val CSRRWI = M"-----------------101-----1110011"
  val CSRRSI = M"-----------------110-----1110011"
  val CSRRCI = M"-----------------111-----1110011"

  val ECALL  = M"00000000000000000000000001110011"
  val EBREAK = M"00000000000100000000000001110011"
  val MRET   = M"00110000001000000000000001110011"

  val MUL    = M"0000001----------000-----0110011"
  val MULH   = M"0000001----------001-----0110011"
  val MULHSU = M"0000001----------010-----0110011"
  val MULHU  = M"0000001----------011-----0110011"
  val DIV    = M"0000001----------100-----0110011"
  val DIVU   = M"0000001----------101-----0110011"
  val REM    = M"0000001----------110-----0110011"
  val REMU   = M"0000001----------111-----0110011"
}

case class Extension(char: Char) {
  assert(char >= 'A' && char <= 'Z')
}

object Extension {
  def apply(baseIsa: BaseIsa): Extension = new Extension(baseIsa match {
    case BaseIsa.RV32E => 'E'
    case _ => 'I'
  })
}

/**
 * The InstructionFormat is the format as specified in the RISC-V specs. This is
 * only used by the decoder to decide how to decode the immediate (since RSx and
 * RD are always in the same position, they can be decoded unconditionally).
 */
sealed trait InstructionFormat

object InstructionFormat {
  case object R extends InstructionFormat
  case object I extends InstructionFormat
  case object S extends InstructionFormat
  case object B extends InstructionFormat
  case object U extends InstructionFormat
  case object J extends InstructionFormat
}

/**
 * Enum to specify the type of registers in InstructionType. New types can be
 * added by calling newElement(String) on the RegisterType object.
 */
object RegisterType extends SpinalEnum {
  val NONE, GPR = newElement()
}

/**
 * The InstructionType is a combination of an InstructionFormat and a
 * specification of the type of each register. The type of register is mainly
 * used to resolve data hazards correctly. E.g., we don't want to forward FP
 * register values to GPR registers (this could happen since the RSx and RD
 * pipeline registers are used for all register types).
 */
abstract class InstructionType(val format: InstructionFormat,
                               val rs1Type: SpinalEnumElement[RegisterType.type],
                               val rs2Type: SpinalEnumElement[RegisterType.type],
                               val rdType:  SpinalEnumElement[RegisterType.type])

object InstructionType {
  case object R extends InstructionType(InstructionFormat.R, RegisterType.GPR,  RegisterType.GPR,  RegisterType.GPR)
  case object I extends InstructionType(InstructionFormat.I, RegisterType.GPR,  RegisterType.NONE, RegisterType.GPR)
  case object S extends InstructionType(InstructionFormat.S, RegisterType.GPR,  RegisterType.GPR,  RegisterType.NONE)
  case object B extends InstructionType(InstructionFormat.B, RegisterType.GPR,  RegisterType.GPR,  RegisterType.NONE)
  case object U extends InstructionType(InstructionFormat.U, RegisterType.NONE, RegisterType.NONE, RegisterType.GPR)
  case object J extends InstructionType(InstructionFormat.J, RegisterType.NONE, RegisterType.NONE, RegisterType.GPR)
}

abstract class TrapCause(val isInterrupt: Boolean, val code: Int, val mtval: UInt = null)
abstract class InterruptCause(code: Int) extends TrapCause(true, code)
abstract class ExceptionCause(code: Int, mtval: UInt = null) extends TrapCause(false, code, mtval)

object TrapCause {
  case object UserSoftwareInterrupt        extends InterruptCause(0)
  case object SupervisorSoftwareInterrupt  extends InterruptCause(1)
  case object MachineSoftwareInterrupt     extends InterruptCause(3)
  case object UserTimerInterrupt           extends InterruptCause(4)
  case object SupervisorTimerInterrupt     extends InterruptCause(5)
  case object MachineTimerInterrupt        extends InterruptCause(7)
  case object UserExternalInterrupt        extends InterruptCause(8)
  case object SupervisorExternalInterrupt  extends InterruptCause(9)
  case object MachineExternalInterrupt     extends InterruptCause(11)

  case class  InstructionAddressMisaligned(address: UInt) extends ExceptionCause(0, address)
  case class  InstructionAccessFault(address: UInt)       extends ExceptionCause(1, address)
  case class  IllegalInstruction(ir: UInt)                extends ExceptionCause(2, ir)
  case object Breakpoint                                  extends ExceptionCause(3)
  case class  LoadAddressMisaligned(address: UInt)        extends ExceptionCause(4, address)
  case class  LoadAccessFault(address: UInt)              extends ExceptionCause(5, address)
  case class  StoreAddressMisaligned(address: UInt)       extends ExceptionCause(6, address)
  case class  StoreAccessFault(address: UInt)             extends ExceptionCause(7, address)
  case object EnvironmentCallFromUMode                    extends ExceptionCause(8)
  case object EnvironmentCallFromSMode                    extends ExceptionCause(9)
  case object EnvironmentCallFromMMode                    extends ExceptionCause(11)
  case class  InstructionPageFault(address: UInt)         extends ExceptionCause(12, address)
  case class  LoadPageFault(address: UInt)                extends ExceptionCause(13, address)
  case class  StorePageFault(address: UInt)               extends ExceptionCause(15, address)
}
