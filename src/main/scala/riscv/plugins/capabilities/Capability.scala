package riscv.plugins.capabilities

import spinal.core._

case class Capability(implicit context: Context) extends Bundle {
  val tag = Bool

  private val xlen = context.config.xlen
  val base = UInt(xlen bits)
  val offset = UInt(xlen bits)
  val length = UInt(xlen bits)

  val perms = Bits(16 bits)

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
    cap.perms := 0
    cap
  }
}
