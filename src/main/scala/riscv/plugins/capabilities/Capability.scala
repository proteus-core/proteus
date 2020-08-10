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

class BaseCapability(implicit context: Context) extends Bundle {
  val tag = Bool

  private val xlen = context.config.xlen
  val base = UInt(xlen bits)
  val length = UInt(xlen bits)

  val perms = Permissions()

  def top = base + length

  def :=(other: Capability): Unit = {
    assignAllByName(other)
  }
}

case class NonPointerCapability(implicit context: Context) extends BaseCapability

case class Capability(implicit context: Context) extends BaseCapability {
  val offset = UInt(context.config.xlen bits)

  def address = base + offset

  def assignFrom(base: BaseCapability, offset: UInt) = {
    assignSomeByName(base)
    this.offset := offset
  }
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
