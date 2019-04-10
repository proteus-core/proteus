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

private class Mstatus(implicit config: Config) extends Csr {
  assert(config.xlen == 32, "mstatus only supported for 32-bit")

  val uie = False
  val sie = False
  val mie = Reg(Bool()).init(False)
  val upie = False
  val spie = False
  val mpie = Reg(Bool()).init(False)
  val spp = False
  val mpp = B"00"
  val fs = B"00"
  val xs = B"00"
  val mprv = False
  val sum = False
  val mxr = False
  val tvm = False
  val tw = False
  val tsr = False
  val sd = False

  val mstatus = sd ## B(0, 8 bits) ## tsr ## tw ## tvm ## mxr ## sum ## mprv ##
                xs ## fs ## mpp ## B"00" ## spp ## mpie ## False ## spie ##
                upie ## mie ## False ## sie ## uie

  val writableRanges = Map[Range, BaseType](
    (3 downto 3) -> mie
  )

  for ((range, reg) <- writableRanges) {
    // The assignment in write() doesn't check the bit width (since we need to
    // convert types and there seems to be no way to do that without also
    // converting bit widths) so we do that manually here.
    assert(range.length == reg.getBitsWidth)
  }

  override def read(): UInt = mstatus.asUInt

  override def write(value: UInt): Unit = {
    for ((range, reg) <- writableRanges) {
      reg := value(range).as(reg)
    }
  }
}

private class Mscratch(implicit config: Config) extends Csr {
  val scratch = Reg(UInt(config.xlen bits))

  override def read(): UInt = scratch
  override def write(value: UInt): Unit = scratch := value
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

private class Mhartid(implicit config: Config) extends Csr {
  override def read(): UInt = U(0, config.xlen bits)
}

class MachineMode(implicit config: Config) extends Plugin {
  override def setup(pipeline: Pipeline): Unit = {
    val csr = pipeline.getService[CsrService]

    csr.registerCsr(pipeline, 0xF11, new Mvendorid)
    csr.registerCsr(pipeline, 0xF12, new Marchid)
    csr.registerCsr(pipeline, 0xF13, new Mimpid)
    csr.registerCsr(pipeline, 0xF14, new Mhartid)

    csr.registerCsr(pipeline, 0x300, new Mstatus)
    csr.registerCsr(pipeline, 0x301, new Misa)

    csr.registerCsr(pipeline, 0x340, new Mscratch)
  }
}
