package riscv.plugins.cheri

import riscv._

import spinal.core._

private class RegScr(implicit context: Context) extends Area with Scr {
  val reg = Reg(PackedCapability()).init(PackedCapability.Null)

  override val needAsr: Boolean = true
  override def read(): Capability = reg
  override def write(value: Capability): Unit = reg.assignFrom(value)
}

class MachineMode(implicit context: Context) extends Plugin[Pipeline] {
  override def setup(): Unit = {
    val scrService = pipeline.getService[ScrService]

    scrService.registerScr(ScrIndex.MTDC, new RegScr)
    scrService.registerScr(ScrIndex.MScratchC, new RegScr)
  }
}
