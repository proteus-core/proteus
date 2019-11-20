package riscv

import spinal.core._
import spinal.lib._

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

  protected val decoderConfig: DecoderConfig
  protected def stage: Stage

  def configure(f: DecoderConfig => Unit): Unit = {
    stage.rework(f(decoderConfig))
  }

  def getSupportedOpcodes: Iterable[MaskedLiteral]
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

  def addOperation(opcode: MaskedLiteral,
                   op: SpinalEnumElement[AluOp.type],
                   src1: SpinalEnumElement[Src1Select.type],
                   src2: SpinalEnumElement[Src2Select.type]): Unit

  def resultData: PipelineData[UInt]
}

trait JumpService {
  def jump(stage: Stage, target: UInt, isTrap: Boolean = false): Unit
}

trait TrapService {
  def trap(stage: Stage, cause: TrapCause): Unit
  def hasTrapped(stage: Stage): Bool
  def hasException(stage: Stage): Bool
  def hasInterrupt(stage: Stage): Bool
}

trait Csr extends Area {
  def read(): UInt

  /**
    * Called for internal writes through the CsrIo interface.
    */
  def write(value: UInt): Unit = assert(false, "Cannot write RO CSR")

  /**
    * Called for software writes (CSR* instructions).
    */
  def swWrite(value: UInt): Unit = write(value)
}

class CsrIo(implicit config: Config) extends Bundle with IMasterSlave {
  val rdata, wdata = UInt(config.xlen bits)
  val write = Bool()

  def read(): UInt = rdata

  def write(value: UInt): Unit = {
    write := True
    wdata := value
  }

  override def asMaster(): Unit = {
    in(wdata, write)
    out(rdata)
  }

  override def asSlave(): Unit = {
    super.asSlave()

    write := False
    write.allowOverride
    wdata.assignDontCare()
    wdata.allowOverride
  }
}

trait CsrService {
  def registerCsr[T <: Csr](id: Int, reg: => T): T
  def getCsr(id: Int): CsrIo
}

class MachineTimerIo extends Bundle with IMasterSlave {
  val update = Bool()
  val interruptPending = Bool()

  def init(): Unit = {
    if (isMasterInterface) {
      update := False
      interruptPending := False
    }
  }

  def postInterrupt(): Unit = {
    assert(isMasterInterface)

    update := True
    interruptPending := True
  }

  def clearInterrupt(): Unit = {
    assert(isMasterInterface)

    update := True
    interruptPending := False
  }

  override def asMaster(): Unit = {
    out(update, interruptPending)
  }
}

trait InterruptService {
  def getMachineTimerIo: MachineTimerIo
}

trait FormalService {
  def lsuDefault(stage: Stage)
  def lsuOnLoad(stage: Stage, addr: UInt, rmask: Bits, rdata: UInt)
  def lsuOnStore(stage: Stage, addr: UInt, wmask: Bits, wdata: UInt)
  def lsuOnMisaligned(stage: Stage): Unit
}
