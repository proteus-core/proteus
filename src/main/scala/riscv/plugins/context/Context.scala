package riscv.plugins.context

import riscv._
import spinal.core._

class Context extends Plugin[DynamicPipeline] with ContextService {

  /**
    *
      [x] Tag memory loads coming from a predefined address range
      [x] Saving the SECRET_VALUE bit in the ROB: maybe nothing?
      [x] Forwarding the SECRET_VALUE bit from loads
          [x] Know whether we're transient: forward BU_IS_BRANCH on dispatch, look for it (+ not having a value) same as for stores
          [x] CDB: new bundle type, explicitly adding SECRET register?
          [x] RDB: maybe nothing needs to be done?
      [x] Propagating/stalling on SECRET_VALUE in reservation stations: checking secret bit if value comes from rob or cdb, only resuming if 0
      [x] Resetting SECRET_VALUE in the ROB after a branch has been resolved,
          [ ] notify waiting reservation stations (many cdb messages? or a different bus for retired branches?)
      [ ] Make the defense optional
      [ ] Operations should not be unnecessarily delayed (after the branch is confirmed well-predicted)
    */

  private object PrivateRegs {
    object SECRET_VALUE extends PipelineData(Bool())
  }

  def isSecret(address: UInt): Bool = {
    address % 8 === 0  // TODO: how to determine what is secret?
  }

  override def setup(): Unit = {
    val lsu = pipeline.getService[LsuService]
    lsu.setAddressTranslator(new LsuAddressTranslator {
      override def translate(stage: Stage, address: UInt, operation: SpinalEnumCraft[LsuOperationType.type], width: SpinalEnumCraft[LsuAccessWidth.type]): UInt = {
        stage.output(PrivateRegs.SECRET_VALUE) := operation === LsuOperationType.LOAD && isSecret(address)
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
    bundle.addElement(PrivateRegs.SECRET_VALUE.asInstanceOf[PipelineData[Data]], PrivateRegs.SECRET_VALUE.dataType)
  }
}
