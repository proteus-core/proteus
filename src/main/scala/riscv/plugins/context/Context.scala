package riscv.plugins.context

import riscv._
import spinal.core._

class Context extends Plugin[Pipeline] with ContextService {

  /**
    *
      [x] Tag memory loads coming from a predefined address range
      [ ] Saving the SECRET_VALUE bit in the ROB: maybe nothing?
      [ ] Forwarding the SECRET_VALUE bit from loads
          [ ] Know whether we're transient: forward BU_IS_BRANCH on dispatch, look for it (+ not having a value) same as for stores
          [ ] CDB: new bundle type, explicitly adding SECRET register?
          [ ] RDB: maybe nothing needs to be done?
      [ ] Propagating/stalling on SECRET_VALUE in reservation stations: checking secret bit if value comes from rob or cdb, only resuming if 0
    */

  private object PrivateRegs {
    object SECRET_VALUE extends PipelineData(Bool())
  }

  def isSecret(address: UInt): Bool = {
    address % 2 === 1
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

  override def isTransientSecret(stage: Stage): Bool = {
    stage.value(PrivateRegs.SECRET_VALUE)
  }
}
