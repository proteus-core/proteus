package riscv.plugins.cheri

import riscv._

import spinal.core._

class Ccsr extends Plugin[Pipeline] {

  override def setup(): Unit = {
    val csrFile = pipeline.service[CsrService]

    csrFile.registerCsr(0xbc0, new Csr {
      val cause = Reg(UInt(5 bits)).init(0)
      val capIdx = Reg(UInt(6 bits)).init(0)

      override def read(): UInt = {
        (capIdx ## cause ## B"000" ## B"11").asUInt.resize(config.xlen bits)
      }

      override def write(value: UInt): Unit = {
        cause := value(9 downto 5)
        capIdx := value(15 downto 10)
      }
    })
  }
}
