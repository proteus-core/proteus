package riscv.plugins.capabilities

import spinal.core._

case class Permissions() extends Bundle {
  val execute = Bool()
  val load = Bool()
  val store = Bool()
  val loadCapability = Bool()
  val storeCapability = Bool()
  val accessSystemRegisters = Bool()

  def allowAll() = setAll(True)
  def allowNone() = setAll(False)

  private def setAll(value: Bool) = {
    for ((_, element) <- elements) {
      element := value
    }
  }
}

case class Capability(implicit context: Context) extends Bundle {
  val tag = Bool

  private val xlen = context.config.xlen
  val base = UInt(xlen bits)
  val offset = UInt(xlen bits)
  val length = UInt(xlen bits)

  val perms = Permissions()

  def address = base + offset
  def top = base + length
}

object Capability {
  def Null(implicit context: Context): Capability = {
    val cap = Capability()
    cap.tag := False
    cap.base := 0
    cap.offset := 0
    cap.length := 0
    cap.perms.allowNone()
    cap
  }
}
