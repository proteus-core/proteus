package riscv.plugins.prospect

import riscv._
import spinal.core._

class Prospect extends Plugin[DynamicPipeline] with ProspectService {

  private var secretRegisters: Vec[Bool] = null

  private object PrivateRegs {
    object SECRET_VALUE extends PipelineData(Bool())
  }

  // randomly selected CSR ids
  private val CSR_PSPLOW = 0x707
  private val CSR_PSPHIGH = 0x708

  private class csrLow(implicit config: Config) extends Csr {

    val addr = Reg(UInt(config.xlen bits)).init(0)
    override def read(): UInt = addr
    override def write(addr: UInt): Unit = addr := addr
  }

  private class csrHigh(implicit config: Config) extends Csr {

    val addr = Reg(UInt(config.xlen bits)).init(0)
    override def read(): UInt = addr
    override def write(addr: UInt): Unit = addr := addr
  }

  def readOnlyCsr(csrId: Int): CsrIo = {
    val csrService = pipeline.service[CsrService]
    val csr = csrService.getCsr(csrId)
    csr.write := False
    csr.wdata.assignDontCare()
    csr
  }

  def isSecret(address: UInt): Bool = {
    val lowCsr = readOnlyCsr(CSR_PSPLOW)
    val highCsr = readOnlyCsr(CSR_PSPHIGH)

    address >= lowCsr.read() && address < highCsr.read()
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      csrService.registerCsr(CSR_PSPLOW, new csrLow)
      csrService.registerCsr(CSR_PSPHIGH, new csrHigh)
    }

    val lsu = pipeline.service[LsuService]
    lsu.setAddressTranslator(new LsuAddressTranslator {
      override def translate(
          stage: Stage,
          address: UInt,
          operation: SpinalEnumCraft[LsuOperationType.type],
          width: SpinalEnumCraft[LsuAccessWidth.type]
      ): UInt = {
        stage.output(PrivateRegs.SECRET_VALUE) := operation === LsuOperationType.LOAD && isSecret(
          address
        )
        address
      }
    })
  }

  override def build(): Unit = {
    pipeline.issuePipeline.stages.last plug new Area {
      pipeline.issuePipeline.stages.last.output(PrivateRegs.SECRET_VALUE) := False
    }
    // TODO: HACK
    val ret = pipeline.retirementStage
    isSecret(ret)

    pipeline plug new Area {
      val regArray = Vec.fill(config.numRegs)(RegInit(False))
      secretRegisters = regArray
    }
  }

  override def isSecret(stage: Stage): Bool = {
    stage.output(PrivateRegs.SECRET_VALUE)
  }

  override def isSecretPipelineReg(reg: PipelineData[Data]): Boolean = {
    reg == PrivateRegs.SECRET_VALUE
  }

  override def isSecretOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](PrivateRegs.SECRET_VALUE.asInstanceOf[PipelineData[Data]])
  }

  override def addSecretToBundle(bundle: DynBundle[PipelineData[Data]]): Unit = {
    bundle.addElement(
      PrivateRegs.SECRET_VALUE.asInstanceOf[PipelineData[Data]],
      PrivateRegs.SECRET_VALUE.dataType
    )
  }

  override def setSecretRegister(regId: UInt, secret: Bool): Unit = {
    secretRegisters(regId) := secret
  }

  override def isSecretRegister(regId: UInt): Bool = {
    secretRegisters(regId)
  }
}
