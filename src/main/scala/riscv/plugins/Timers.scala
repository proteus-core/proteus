package riscv.plugins

import riscv._

import spinal.core._

class Timers extends Plugin[Pipeline] {
  override def setup(): Unit = {
    val csr = pipeline.service[CsrService]

    val cycleCsr = csr.registerCsr(0xC00, new Csr {
      val cycle = Reg(UInt(64 bits)).init(0)
      cycle := cycle + 1

      override def read(): UInt = {
        if (config.xlen == 32) {
          cycle(31 downto 0)
        } else {
          cycle
        }
      }
    })

    // For now, just alias time[h] to cycle[h]
    csr.registerCsr(0xC01, cycleCsr)

    val instretCsr = csr.registerCsr(0xC02, new Csr {
      val incInstret = in Bool()
      val instret = Reg(UInt(64 bits)).init(0)

      when (incInstret) {
        instret := instret + 1
      }

      override def read(): UInt = {
        if (config.xlen == 32) {
          instret(31 downto 0)
        } else {
          instret
        }
      }
    })

    pipeline plug new Area {
      instretCsr.incInstret := pipeline.retirementStage.arbitration.isDone
    }

    if (config.xlen == 32) {
      val cyclehCsr = csr.registerCsr(0xC80, new Csr {
        override def read(): UInt = cycleCsr.cycle(63 downto 32)
      })

      csr.registerCsr(0xC81, cyclehCsr)

      csr.registerCsr(0xC82, new Csr {
        override def read(): UInt = instretCsr.instret(63 downto 32)
      })
    }
  }
}
