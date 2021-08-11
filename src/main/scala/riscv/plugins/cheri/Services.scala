package riscv.plugins.cheri

import riscv._

import spinal.core._
import spinal.lib._

trait PccService {
  def getPcc(stage: Stage): Capability
  def jump(stage: Stage, pcc: Capability, capIdx: CapIdx): Unit
}

trait Scr {
  val needAsr: Boolean

  def read(): Capability
  def write(value: Capability)
}

case class ScrIo(implicit context: Context) extends Bundle with IMasterSlave {
  val valid = Bool()
  val id = UInt(5 bits)
  val write = Bool()
  val wdata = PackedCapability()
  val rdata = PackedCapability()
  val unknownId = Bool()
  val hasAsr = Bool()
  val needAsr = Bool()

  override def asMaster(): Unit = {
    in(valid, id, write, wdata, hasAsr)
    out(rdata, unknownId, needAsr)
  }
}

trait ScrService {
  /**
    * Get an IO port for accessing the SCR file.
    *
    * This has to be called during the setup phase.
    *
    * Note that when multiple stages create IO ports, it's undefined which one "wins" when more than
    * one tries to access it. Therefore, plugins using this method should somehow ensure that they
    * have exclusive access, e.g., by using [[ScheduleService.claimPipeline()]].
    */
  def getIo(stage: Stage): ScrIo
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability

  def registerScr[T <: Scr](id: Int, scr: => T): T

  /**
    * Register a new SCR that extends the given CSR. Extending a CSR means that
    * reads and write of the CSR will refer to this SCR's offset.
    */
  def registerScr[T <: Scr](id: Int, offsetCsr: Int, scr: => T): T
  def getScr(stage: Stage, id: Int): Scr
  def getRegisteredScrs: Seq[Int]
}

trait ExceptionService {
  protected def handleException(stage: Stage, cause: UInt, capIdx: CapIdx): Unit

  def except(stage: Stage, cause: UInt, capIdx: CapIdx): Unit = {
    handleException(stage, cause, capIdx)
  }

  def except(stage: Stage, cause: ExceptionCause, capIdx: CapIdx): Unit = {
    handleException(stage, U(cause.code), capIdx)
  }
}

trait TaggedMemoryService {
  def createCapBus(stage: Stage): CapBus
}
