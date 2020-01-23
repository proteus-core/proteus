package riscv.soc.devices

import riscv._
import riscv.soc._

import spinal.core._
import spinal.lib._

class MachineTimers(pipeline: Pipeline)(implicit config: Config) extends MmioDevice {
  private val mtime = Reg(UInt(64 bits)).init(0)
  mtime := mtime + 1

  private val mtimecmp = Reg(UInt(64 bits)).init(0)

  val mtimerIo = master(new IrqIo)
  mtimerIo.setName("mtimer")
  mtimerIo.init()

  parent rework {
    pipeline.getService[InterruptService].getMachineTimerIrqIo <> mtimerIo
  }

  // It is important that this comes before the call to build() to ensure that
  // the clearing of the mtip bit on a write has precedence over setting it.
  when (mtime >= mtimecmp) {
    mtimerIo.postInterrupt()
  }

  addRegister(0, mtime)

  addRegister(8, new MmioRegister {
    override val width: BitCount = mtimecmp.getBitsWidth bits

    override def read(): UInt = mtimecmp

    override def write(value: UInt): Unit = {
      mtimecmp := value
      mtimerIo.clearInterrupt()
    }
  })

  build()
}
