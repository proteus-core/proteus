package riscv.plugins.cheri

import riscv._

import spinal.core._

object Opcodes {
  val CGetPerm        = M"111111100000-----000-----1011011"
  val CGetType        = M"111111100001-----000-----1011011"
  val CGetBase        = M"111111100010-----000-----1011011"
  val CGetLen         = M"111111100011-----000-----1011011"
  val CGetTag         = M"111111100100-----000-----1011011"
  val CGetSealed      = M"111111100101-----000-----1011011"
  val CGetOffset      = M"111111100110-----000-----1011011"
  val CGetAddr        = M"111111101111-----000-----1011011"

  val CSeal           = M"0001011----------000-----1011011"
  val CUnseal         = M"0001100----------000-----1011011"
  val CAndPerm        = M"0001101----------000-----1011011"
  val CSetOffset      = M"0001111----------000-----1011011"
  val CSetAddr        = M"0010000----------000-----1011011"
  val CIncOffset      = M"0010001----------000-----1011011"
  val CIncOffsetImm   = M"-----------------001-----1011011"
  val CSetBounds      = M"0001000----------000-----1011011"
  val CSetBoundsExact = M"0001001----------000-----1011011"
  val CSetBoundsImm   = M"-----------------010-----1011011"
  val CClearTag       = M"111111101011-----000-----1011011"

  val CMove           = M"111111101010-----000-----1011011"
  val CSpecialRW      = M"0000001----------000-----1011011"

  val CJALR           = M"111111101100-----000-----1011011"
  val CInvoke         = M"1111110----------000000011011011"

  val LB_CAP          = M"111110101000-----000-----1011011"
  val LH_CAP          = M"111110101001-----000-----1011011"
  val LW_CAP          = M"111110101010-----000-----1011011"
  val LC_CAP          = M"111110101011-----000-----1011011"
  val LBU_CAP         = M"111110101100-----000-----1011011"
  val LHU_CAP         = M"111110101101-----000-----1011011"

  val SB_CAP          = M"1111100----------000010001011011"
  val SH_CAP          = M"1111100----------000010011011011"
  val SW_CAP          = M"1111100----------000010101011011"
  val SC_CAP          = M"1111100----------000010111011011"

  val LC              = M"-----------------011-----0000011"
  val SC              = M"-----------------011-----0100011"
}

object RegisterType {
  val CAP = riscv.RegisterType.newElement("CAP")
}

object InstructionType {
  // Naming scheme: <FORMAT>_<RS1_TYPE><RS2_TYPE><RD_TYPE>
  // Where register type can be R (GPR), C (CAP), or x (NONE)
  case object R_CxR extends InstructionType(InstructionFormat.R, RegisterType.CAP,       riscv.RegisterType.NONE, riscv.RegisterType.GPR)
  case object R_CRC extends InstructionType(InstructionFormat.R, RegisterType.CAP,       riscv.RegisterType.GPR,  RegisterType.CAP)
  case object R_CRx extends InstructionType(InstructionFormat.R, RegisterType.CAP,       riscv.RegisterType.GPR,  riscv.RegisterType.NONE)
  case object R_CxC extends InstructionType(InstructionFormat.R, RegisterType.CAP,       riscv.RegisterType.NONE, RegisterType.CAP)
  case object R_CCx extends InstructionType(InstructionFormat.R, RegisterType.CAP,       RegisterType.CAP,        riscv.RegisterType.NONE)
  case object R_CCC extends InstructionType(InstructionFormat.R, RegisterType.CAP,       RegisterType.CAP,        RegisterType.CAP)
  case object I_CxC extends InstructionType(InstructionFormat.I, RegisterType.CAP,       riscv.RegisterType.NONE, RegisterType.CAP)
  case object I_RxC extends InstructionType(InstructionFormat.I, riscv.RegisterType.GPR, riscv.RegisterType.NONE, RegisterType.CAP)
  case object S_RCx extends InstructionType(InstructionFormat.S, riscv.RegisterType.GPR, RegisterType.CAP,        riscv.RegisterType.NONE)
}

object ScrIndex {
  val PCC = 0
  val DDC = 1

  val UTCC = 4
  val UTDC = 5
  val UScratchC = 6
  val UEPCC = 7

  val STCC = 12
  val STDC = 13
  val SScratchC = 14
  val SEPCC = 15

  val MTCC = 28
  val MTDC = 29
  val MScratchC = 30
  val MEPCC = 31
}

object TrapCause {
  case object CheriException extends riscv.ExceptionCause(10) // FIXME: 32
}

sealed abstract class ExceptionCause(val code: Int)

object ExceptionCause {
  case object None                                extends ExceptionCause(0x00)
  case object LengthViolation                     extends ExceptionCause(0x01)
  case object TagViolation                        extends ExceptionCause(0x02)
  case object SealViolation                       extends ExceptionCause(0x03)
  case object TypeViolation                       extends ExceptionCause(0x04)
  case object CallTrap                            extends ExceptionCause(0x05)
  case object ReturnTrap                          extends ExceptionCause(0x06)
  case object TrustedSystemStackOverflow          extends ExceptionCause(0x07)
  case object SoftwareDefinedPermissionViolation  extends ExceptionCause(0x08)
  case object MmuProhibitsStoreCapability         extends ExceptionCause(0x09)
  case object BoundsCannotBeRepresentedExactly    extends ExceptionCause(0x0a)
  case object GlobalViolation                     extends ExceptionCause(0x10)
  case object PermitExecuteViolation              extends ExceptionCause(0x11)
  case object PermitLoadViolation                 extends ExceptionCause(0x12)
  case object PermitStoreViolation                extends ExceptionCause(0x13)
  case object PermitLoadCapabilityViolation       extends ExceptionCause(0x14)
  case object PermitStoreCapabilityViolation      extends ExceptionCause(0x15)
  case object PermitStoreLocalCapabilityViolation extends ExceptionCause(0x16)
  case object PermitSealViolation                 extends ExceptionCause(0x17)
  case object AccessSystemRegistersViolation      extends ExceptionCause(0x18)
  case object PermitCInvokeViolation              extends ExceptionCause(0x19)
  case object AccessCInvokeIdcViolation           extends ExceptionCause(0x1a)
  case object PermitUnsealViolation               extends ExceptionCause(0x1b)
  case object PermitSetCidViolation               extends ExceptionCause(0x1c)
}
