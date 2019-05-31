package riscv.soc.devices

import riscv._
import riscv.soc._

import spinal.core._

class MachineTimers(implicit config: Config) extends MmioDevice {
  private val mtime = Reg(UInt(64 bits)).init(0)
  mtime := mtime + 1

  private val mtimecmp = Reg(UInt(64 bits)).init(0)

  addRegister(0, mtime)
  addRegister(8, mtimecmp)
  build()
}
