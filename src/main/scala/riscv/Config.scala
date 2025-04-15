package riscv

import spinal.core.log2Up

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

class Config(val baseIsa: BaseIsa, val debug: Boolean = true, val stlSpec: Boolean = true) {
  def xlen = baseIsa.xlen
  def numRegs = baseIsa.numRegs

  def memBusWidth: Int = 128

  def robEntries: Int = 32

  def parallelAlus: Int = 8

  def parallelMulDivs: Int = 2

  def parallelLoads: Int = 3

  def addressBasedPsf: Boolean = true

  def addressBasedSsb: Boolean = true

  def ibusConfig = MemBusConfig(
    addressWidth = baseIsa.xlen,
    idWidth = 2,
    dataWidth = memBusWidth,
    readWrite = false
  )
  def readDbusConfig = MemBusConfig(
    addressWidth = baseIsa.xlen,
    idWidth = log2Up(parallelLoads + 1),
    dataWidth = memBusWidth,
    readWrite = false
  )
  def dbusConfig = MemBusConfig(
    addressWidth = baseIsa.xlen,
    idWidth = log2Up(parallelLoads + 1),
    dataWidth = memBusWidth
  )
}
