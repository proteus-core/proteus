package riscv.soc

import riscv._

import spinal.core._
import spinal.lib._

import scala.collection.mutable

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

  val dbus = master(new MemBus(config.dbusConfig))
  dbus.cmd.ready := True
  dbus.rsp.valid := False
  dbus.rsp.rdata.assignDontCare()

  protected def build() {
    when (dbus.cmd.valid) {
      switch (dbus.cmd.address) {
        for ((regOffset, reg) <- registers) {
          if (reg.width.value == 32) {
            is (regOffset) {
              when (dbus.cmd.write) {
                reg.write(dbus.cmd.wdata)
              }.otherwise {
                // TODO: Wait for bus.rsp.ready?
                dbus.rsp.valid := True
                dbus.rsp.rdata := reg.read()
              }
            }
          } else if (reg.width.value == 64) {
            is (regOffset) {
              when (dbus.cmd.write) {
                reg.write(reg.read()(63 downto 32) @@ dbus.cmd.wdata)
              }.otherwise {
                // TODO: Wait for bus.rsp.ready?
                dbus.rsp.valid := True
                dbus.rsp.rdata := reg.read()(31 downto 0)
              }
            }
            is (regOffset + 4) {
              when (dbus.cmd.write) {
                reg.write(dbus.cmd.wdata @@ reg.read()(31 downto 0))
              }.otherwise {
                // TODO: Wait for bus.rsp.ready?
                dbus.rsp.valid := True
                dbus.rsp.rdata := reg.read()(63 downto 32)
              }
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
