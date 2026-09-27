package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class SecretRegionTracking(implicit config: Config)
    extends Plugin[DynamicPipeline]
    with SecretRegionService {
  private val CSR_PSPLOW = 0x707
  private val CSR_PSPHIGH = 0x708
  private val CSR_PSP2LOW = 0x709
  private val CSR_PSP2HIGH = 0x70a

  private class csrLow(implicit config: Config) extends Csr {
    val addr = Reg(UInt(config.xlen bits)).init(0)
    override def read(): UInt = addr
    override def write(addr: UInt): Unit = this.addr := addr
  }

  private class csrHigh(implicit config: Config) extends Csr {
    val addr = Reg(UInt(config.xlen bits)).init(0)
    override def read(): UInt = addr
    override def write(addr: UInt): Unit = this.addr := addr
  }

  def readOnlyCsr(csrId: Int): CsrIo = {
    val csrIn = slave(new CsrIo)

    val pipelineArea = pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      val csr = csrService.getCsr(csrId)
      csr <> csrIn
    }

    csrIn.write := False
    csrIn.wdata.assignDontCare()
    csrIn
  }

  def isSecret(address: UInt): Bool = {
    val lowCsr = readOnlyCsr(CSR_PSPLOW)
    val highCsr = readOnlyCsr(CSR_PSPHIGH)
    val lowCsr2 = readOnlyCsr(CSR_PSP2LOW)
    val highCsr2 = readOnlyCsr(CSR_PSP2HIGH)

    (address >= lowCsr.read() && address < highCsr.read()) || (address >= lowCsr2
      .read() && address < highCsr2.read())
  }

  override def setup(): Unit = {
    assert(pipeline.hasService[PipelineTaintService], "Taint tracking required for secret region tracking")

    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      csrService.registerCsr(CSR_PSPLOW, new csrLow)
      csrService.registerCsr(CSR_PSPHIGH, new csrHigh)
      csrService.registerCsr(CSR_PSP2LOW, new csrLow)
      csrService.registerCsr(CSR_PSP2HIGH, new csrHigh)
    }

    val lsu = pipeline.service[LsuService]
    lsu.setAddressTranslator(new LsuAddressTranslator {
      override def translate(
                              stage: Stage,
                              address: UInt,
                              operation: SpinalEnumCraft[LsuOperationType.type],
                              width: SpinalEnumCraft[LsuAccessWidth.type]
                            ): UInt = {
        pipeline
          .service[PipelineTaintService]
          .tainted(stage) := operation === LsuOperationType.LOAD && isSecret(address)
        address
      }
    })
  }
}
