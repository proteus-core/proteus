package riscv.plugins.cheri

import spinal.core._

class CapIdx extends Bundle {
  private val idx = Bits(6 bits)

  def assignFromScr(scr: UInt): Unit = {
    idx.msb := True
    idx(4 downto 0) := scr.asBits
  }

  def assignFromGpcr(cr: UInt): Unit = {
    assert(cr.getBitsWidth <= 5)
    idx := cr.asBits.resize(getBitsWidth bits)
  }

  def asUInt: UInt = idx.asUInt
}

object CapIdx {
  def apply(): CapIdx = new CapIdx

  def scr(scr: UInt): CapIdx = {
    val idx = new CapIdx()
    idx.assignFromScr(scr)
    idx
  }

  def scr(scr: Int): CapIdx = {
    this.scr(U(scr, 5 bits))
  }

  def gpcr(cr: UInt): CapIdx = {
    val idx = new CapIdx()
    idx.assignFromGpcr(cr)
    idx
  }
}
