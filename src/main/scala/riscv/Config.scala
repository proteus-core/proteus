package riscv

sealed trait BaseIsa {
  val xlen: Int
  val numRegs: Int
}

object BaseIsa {
  case object RV32I extends BaseIsa {
    override val xlen = 32
    override val numRegs = 32
  }
  case object RV32E extends BaseIsa {
    override val xlen = 32
    override val numRegs = 16
  }
  case object RV64I extends BaseIsa {
    override val xlen = 64
    override val numRegs = 32
  }
}

class Config(val baseIsa: BaseIsa) {
  def xlen = baseIsa.xlen
  def numRegs = baseIsa.xlen

  def ibusConfig = MemBusConfig(baseIsa.xlen, false)
  def dbusConfig = MemBusConfig(baseIsa.xlen)
}
