package riscv

import spinal.core._

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

sealed abstract class TrapCause(val isInterrupt: Boolean, val code: Int, val mtval: UInt = null)
sealed abstract class InterruptCause(code: Int) extends TrapCause(true, code)
sealed abstract class ExceptionCause(code: Int, mtval: UInt = null) extends TrapCause(false, code, mtval)

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
