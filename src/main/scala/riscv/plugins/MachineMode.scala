package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

private class Misa(pipeline: Pipeline) extends Csr {
  val xlen = pipeline.config.xlen

  val mxlVal = xlen match {
    case 32 => 1
    case 64 => 2
  }

  val mxl = U(mxlVal, 2 bits)
  val extensions = U(0, 26 bits)

  for (extension <- pipeline.getImplementedExtensions) {
    extensions(extension.char - 'A') := True
  }

  val filling = U(0, xlen - 28 bits)

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
  val mpp = B"11"
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
    (3 downto 3) -> mie,
    (7 downto 7) -> mpie
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

private class Mtvec(implicit config: Config) extends Csr {
  val mtvec = Reg(UInt(config.xlen bits))
  val base = mtvec(config.xlen - 1 downto 2) << 2
  val mode = mtvec(1 downto 0)

  override def read(): UInt = mtvec
  override def write(value: UInt): Unit = mtvec := value
}

private class Mscratch(implicit config: Config) extends Csr {
  val scratch = Reg(UInt(config.xlen bits))

  override def read(): UInt = scratch
  override def write(value: UInt): Unit = scratch := value
}

private class Mepc(implicit config: Config) extends Csr {
  val epc = Reg(UInt(config.xlen - 2 bits))

  override def read(): UInt = epc << 2
  override def write(value: UInt): Unit = epc := value >> 2
}

private class Mcause(implicit config: Config) extends Csr {
  val interrupt = Reg(Bool()).init(False)
  val cause = Reg(UInt(4 bits)).init(0)
  val mcause = interrupt ## Utils.zeroExtend(cause, config.xlen - 1)

  override def read(): UInt = mcause.asUInt
  override def write(value: UInt): Unit = {
    interrupt := value.msb
    cause := value(3 downto 0)
  }
}

private class Mtval(implicit config: Config) extends Csr {
  val tval = Reg(UInt(config.xlen bits))

  override def read(): UInt = tval
  override def write(value: UInt): Unit = tval := value
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

class MachineMode(ecallStage: Stage,
                  addMtvec: Boolean = true,
                  addMepc: Boolean = true) extends Plugin[Pipeline] {
  object Data {
    object ECALL  extends PipelineData(Bool())
    object EBREAK extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    val csr = pipeline.getService[CsrService]

    csr.registerCsr(0xF11, new Mvendorid)
    csr.registerCsr(0xF12, new Marchid)
    csr.registerCsr(0xF13, new Mimpid)
    csr.registerCsr(0xF14, new Mhartid)

    csr.registerCsr(0x300, new Mstatus)
    csr.registerCsr(0x301, new Misa(pipeline))

    if (addMtvec) {
      csr.registerCsr(0x305, new Mtvec)
    }

    csr.registerCsr(0x340, new Mscratch)

    if (addMepc) {
      csr.registerCsr(0x341, new Mepc)
    }

    csr.registerCsr(0x342, new Mcause)
    csr.registerCsr(0x343, new Mtval)

    val issuer = pipeline.getService[IssueService]

    pipeline.getService[DecoderService].configure {config =>
      config.addDefault(Map(
        Data.ECALL  -> False,
        Data.EBREAK -> False
      ))

      config.addDecoding(Opcodes.ECALL, InstructionType.I,
                         Map(Data.ECALL -> True))
      config.addDecoding(Opcodes.EBREAK, InstructionType.I,
                         Map(Data.EBREAK -> True))

      issuer.setDestination(Opcodes.ECALL, ecallStage)
      issuer.setDestination(Opcodes.EBREAK, ecallStage)

    }
  }

  override def build(): Unit = {
    val ecallArea = ecallStage plug new Area {
      import ecallStage._

      def trap(cause: TrapCause) = {
        val trapHandler = pipeline.getService[TrapService]
        trapHandler.trap(ecallStage, cause)
      }

      when (arbitration.isValid) {
        when (value(Data.ECALL)) {
          trap(TrapCause.EnvironmentCallFromMMode)
        }.elsewhen (value(Data.EBREAK)) {
          trap(TrapCause.Breakpoint)
        }
      }
    }
  }
}
