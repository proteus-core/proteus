package riscv.plugins

import riscv._

import spinal.core._

class Timers(implicit config: Config) extends Plugin {
  override def setup(pipeline: Pipeline): Unit = {
    val csr = pipeline.getService[CsrService]

    val cycleCsr = csr.registerCsr(pipeline, 0xC00, new csr.Register {
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
    csr.registerCsr(pipeline, 0xC01, cycleCsr)

    val instretCsr = csr.registerCsr(pipeline, 0xC02, new csr.Register {
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
      instretCsr.incInstret := pipeline.stages.last.arbitration.isDone
    }

    if (config.xlen == 32) {
      val cyclehCsr = csr.registerCsr(pipeline, 0xC80, new csr.Register {
        override def read(): UInt = cycleCsr.cycle(63 downto 32)
      })

      csr.registerCsr(pipeline, 0xC81, cyclehCsr)

      csr.registerCsr(pipeline, 0xC82, new csr.Register {
        override def read(): UInt = instretCsr.instret(63 downto 32)
      })
    }
  }
}
