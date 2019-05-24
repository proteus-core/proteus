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

sealed abstract class TrapCause(val isInterrupt: Boolean, val code: Int)
sealed abstract class InterruptCause(code: Int) extends TrapCause(true, code)
sealed abstract class ExceptionCause(code: Int) extends TrapCause(false, code)

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

  case object InstructionAddressMisaligned extends ExceptionCause(0)
  case object InstructionAccessFault       extends ExceptionCause(1)
  case object IllegalInstruction           extends ExceptionCause(2)
  case object Breakpoint                   extends ExceptionCause(3)
  case object LoadAddressMisaligned        extends ExceptionCause(4)
  case object LoadAccessFault              extends ExceptionCause(5)
  case object StoreAddressMisaligned       extends ExceptionCause(6)
  case object StoreAccessFault             extends ExceptionCause(7)
  case object EnvironmentCallFromUMode     extends ExceptionCause(8)
  case object EnvironmentCallFromSMode     extends ExceptionCause(9)
  case object EnvironmentCallFromMMode     extends ExceptionCause(11)
  case object InstructionPageFault         extends ExceptionCause(12)
  case object LoadPageFault                extends ExceptionCause(13)
  case object StorePageFault               extends ExceptionCause(15)
}
