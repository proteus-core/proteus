package riscv.plugins

import riscv._

import spinal.core._

class MachineMode(implicit config: Config) extends Plugin {
  override def setup(pipeline: Pipeline): Unit = {
    val csr = pipeline.getService[CsrService]

    csr.registerCsr(pipeline, 0x301, new Csr {
      private val mxlVal = config.xlen match {
        case 32 => 1
        case 64 => 2
      }

      private val mxl = U(mxlVal, 2 bits)
      private val extensions = U(0, 26 bits)

      private val baseExtension = config.baseIsa match {
        case BaseIsa.RV32E => 'E'
        case _ => 'I'
      }

      private val supportedExtensions = Seq(baseExtension)

      for (extension <- supportedExtensions) {
        extensions(extension - 'A') := True
      }

      private val filling = U(0, config.xlen - 28 bits)

      private val misa = mxl ## filling ## extensions

      override def read(): UInt = {
        misa.asUInt
      }

      // Writes are ignored
      override def write(value: UInt): Unit = ()
    })
  }
}
