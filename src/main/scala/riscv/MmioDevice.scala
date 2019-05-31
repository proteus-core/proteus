package riscv

import scala.collection.mutable

import spinal.core._
import spinal.lib._

trait MmioRegister {
  val width: BitCount
  def read(): UInt = U(0, width)
  def write(value: UInt): Unit = ()
}

abstract class MmioDevice(implicit config: Config) extends Component {
  private val registers = mutable.Map[Int, MmioRegister]()

  protected def addRegister(offset: Int, register: MmioRegister): Unit = {
    val regWidth = register.width.value
    assert(regWidth == 32 || regWidth == 64)
    assert(((offset * 8) % regWidth) == 0)

    registers(offset) = register
  }

  protected def addRegister(offset: Int, reg: UInt): Unit = {
    assert(reg.isReg)

    addRegister(offset, new MmioRegister {
      override val width: BitCount = reg.getBitsWidth bits

      override def read(): UInt = reg
      override def write(value: UInt): Unit = reg := value
    })
  }

  val bus = master(new MemBus(config.xlen))
  bus.rdata.assignDontCare()

  protected def build() {
    switch (bus.byteAddress) {
      for ((regOffset, reg) <- registers) {
        if (reg.width.value == 32) {
          is (regOffset) {
            when (bus.read) {
              bus.rdata := reg.read()
            }.elsewhen (bus.write) {
              reg.write(bus.wdata)
            }
          }
        } else if (reg.width.value == 64) {
          is (regOffset) {
            when (bus.read) {
              bus.rdata := reg.read()(31 downto 0)
            }.elsewhen (bus.write) {
              reg.write(reg.read()(63 downto 32) @@ bus.wdata)
            }
          }
          is (regOffset + 4) {
            when (bus.read) {
              bus.rdata := reg.read()(63 downto 32)
            }.elsewhen (bus.write) {
              reg.write(bus.wdata @@ reg.read()(31 downto 0))
            }
          }
        }
      }
    }
  }

  def size: Int = {
    registers.map({case (offset, register) =>
      offset + register.width.value / 8
    }).max
  }
}

class MachineTimers(implicit config: Config) extends MmioDevice {
  private val mtime = Reg(UInt(64 bits)).init(0)
  mtime := mtime + 1

  private val mtimecmp = Reg(UInt(64 bits)).init(0)

  addRegister(0, mtime)
  addRegister(8, mtimecmp)
  build()
}

class CharDev(implicit config: Config) extends MmioDevice {
  val io = master(Flow(UInt(8 bits)))
  io.valid := False
  io.payload.assignDontCare()

  addRegister(0, new MmioRegister {
    override val width: BitCount = 32 bits

    override def write(value: UInt): Unit = {
      io.push(value(7 downto 0))
    }
  })

  build()
}
