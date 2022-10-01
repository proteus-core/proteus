package riscv.plugins.prospect

import riscv._
import spinal.core._

class Prospect extends Plugin[DynamicPipeline] with ProspectService {

  private var secretRegisters: Vec[Bool] = null

  private object PrivateRegs {
    object SECRET_VALUE extends PipelineData(Bool())
  }

  def isSecret(address: UInt): Bool = {
    val lower1 = UInt(config.xlen bits)
    val lower2 = UInt(config.xlen bits)
    val upper1 = UInt(config.xlen bits)
    val upper2 = UInt(config.xlen bits)

    lower1 := U"h8001b9e4"
    upper1 := U"h8001bad8"

    lower2 := U"h8001b930"
    upper2 := U"h8001b958"

    val in1 = address >= lower1 && address < upper1
    val in2 = address >= lower2 && address < upper2
    in1 || in2
//    False
//    True
  }

  override def setup(): Unit = {
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
