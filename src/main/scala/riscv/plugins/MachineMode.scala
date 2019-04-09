package riscv.plugins

import riscv._

import spinal.core._

private class Misa(implicit config: Config) extends Csr {
  val mxlVal = config.xlen match {
    case 32 => 1
    case 64 => 2
  }

  val mxl = U(mxlVal, 2 bits)
  val extensions = U(0, 26 bits)

  val baseExtension = config.baseIsa match {
    case BaseIsa.RV32E => 'E'
    case _ => 'I'
  }

  val supportedExtensions = Seq(baseExtension)

  for (extension <- supportedExtensions) {
    extensions(extension - 'A') := True
  }

  val filling = U(0, config.xlen - 28 bits)

  val misa = mxl ## filling ## extensions

  override def read(): UInt = {
    misa.asUInt
  }

  // Writes are ignored
  override def write(value: UInt): Unit = ()
}

private class Mvendorid(implicit config: Config) extends Csr {
  // 0 can be used for non-commercial implementations
  override def read(): UInt = U(0, config.xlen bits)
}

private class Marchid(implicit config: Config) extends Csr {
  // Implement if we ever apply for an open-source machine architecture ID with
  // the RISC-V foundation.
  override def read(): UInt = U(0, config.xlen bits)
}

private class Mimpid(implicit config: Config) extends Csr {
  // We could use this field to identify different versions of our core.
  override def read(): UInt = U(0, config.xlen bits)
}

class MachineMode(implicit config: Config) extends Plugin {
  override def setup(pipeline: Pipeline): Unit = {
    val csr = pipeline.getService[CsrService]

    csr.registerCsr(pipeline, 0xF11, new Mvendorid)
    csr.registerCsr(pipeline, 0xF12, new Marchid)
    csr.registerCsr(pipeline, 0xF13, new Mimpid)
    csr.registerCsr(pipeline, 0x301, new Misa)
  }
}
