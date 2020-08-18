package riscv.plugins.capabilities

import riscv._

import spinal.core._

class Lsu extends Plugin[Pipeline] {
  override def setup(): Unit = {
    pipeline.getService[LsuService] setAddressTranslator new LsuAddressTranslator {
      override def translate(stage: Stage,
                             address: UInt,
                             operation: SpinalEnumCraft[LsuOperationType.type],
                             width: SpinalEnumCraft[LsuAccessWidth.type]): UInt = {
        val ddc = pipeline.getService[ScrService].getDdc(stage)
        val exceptionHandler = pipeline.getService[ExceptionService]

        val result = ddc.address + address

        val byteWidth = width.mux(
          LsuAccessWidth.B -> U(1),
          LsuAccessWidth.H -> U(2),
          LsuAccessWidth.W -> U(4)
        )

        def except(cause: ExceptionCause) = {
          exceptionHandler.except(stage, cause, ScrIndex.DDC)
        }

        when (stage.arbitration.isValid && operation =/= LsuOperationType.NONE) {
          when (!ddc.tag) {
            except(ExceptionCause.TagViolation)
          } elsewhen (operation === LsuOperationType.LOAD && !ddc.perms.load) {
            except(ExceptionCause.PermitLoadViolation)
          } elsewhen (operation === LsuOperationType.STORE && !ddc.perms.store) {
            except(ExceptionCause.PermitStoreViolation)
          } elsewhen (result + byteWidth > ddc.top) {
            except(ExceptionCause.LengthViolation)
          } elsewhen (result < ddc.base) {
            except(ExceptionCause.LengthViolation)
          }
        }

        result
      }
    }
  }
}
