package riscv.plugins.cheri

import riscv._

import spinal.core._

class Sealing(stage: Stage)(implicit context: Context) extends Plugin[Pipeline] {
  private object Data {
    object CSEAL extends PipelineData(Bool())
    object CUNSEAL extends PipelineData(Bool())
    object CINVOKE extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure {config =>
      config.addDefault(Map(
        Data.CSEAL -> False,
        Data.CUNSEAL -> False,
        Data.CINVOKE -> False
      ))

      config.addDecoding(Opcodes.CSeal, InstructionType.R_CCC, Map(
        Data.CSEAL -> True
      ))

      config.addDecoding(Opcodes.CUnseal, InstructionType.R_CCC, Map(
        Data.CUNSEAL -> True
      ))

      config.addDecoding(Opcodes.CInvoke, InstructionType.R_CCC, Map(
        Data.CINVOKE -> True
      ))
      config.setFixedRegisters(Opcodes.CInvoke, rd = Some(31))
    }
  }

  override def build(): Unit = {
    stage plug new Area {
      import stage._

      val cs1 = value(context.data.CS1_DATA)
      val cs2 = value(context.data.CS2_DATA)
      val cs2Address = cs2.address
      val cs1Idx = CapIdx.gpcr(value(pipeline.data.RS1))
      val cs2Idx = CapIdx.gpcr(value(pipeline.data.RS2))

      def except(cause: ExceptionCause, capIdx: CapIdx) = {
        val handler = pipeline.getService[ExceptionHandler]
        handler.except(stage, cause, capIdx)
      }

      when (arbitration.isValid && value(Data.CSEAL)) {
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        when (!arbitration.isStalled) {
          when (!cs1.tag) {
            except(ExceptionCause.TagViolation, cs1Idx)
          } elsewhen (!cs2.tag) {
            except(ExceptionCause.TagViolation, cs2Idx)
          } elsewhen (cs1.isSealed) {
            except(ExceptionCause.SealViolation, cs1Idx)
          } elsewhen (cs2.isSealed) {
            except(ExceptionCause.SealViolation, cs2Idx)
          } elsewhen (!cs2.perms.seal) {
            except(ExceptionCause.PermitSealViolation, cs2Idx)
          } elsewhen (cs2Address < cs2.base) {
            except(ExceptionCause.LengthViolation, cs2Idx)
          } elsewhen (cs2Address >= cs2.top) {
            except(ExceptionCause.LengthViolation, cs2Idx)
          } elsewhen (cs2Address > context.maxOtype) {
            except(ExceptionCause.LengthViolation, cs2Idx)
          } otherwise {
            val cd = PackedCapability()
            cd.assignFrom(cs1)
            cd.otype.value.allowOverride
            cd.otype.value := cs2Address.resized

            output(context.data.CD_DATA).assignFrom(cd)
            output(pipeline.data.RD_VALID) := True
          }
        }
      }

      when (arbitration.isValid && value(Data.CUNSEAL)) {
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        when (!arbitration.isStalled) {
          when (!cs1.tag) {
            except(ExceptionCause.TagViolation, cs1Idx)
          } elsewhen (!cs2.tag) {
            except(ExceptionCause.TagViolation, cs2Idx)
          } elsewhen (!cs1.isSealed) {
            except(ExceptionCause.SealViolation, cs1Idx)
          } elsewhen (cs2.isSealed) {
            except(ExceptionCause.SealViolation, cs2Idx)
          } elsewhen (cs2Address =/= cs1.otype.value) {
            except(ExceptionCause.TypeViolation, cs2Idx)
          } elsewhen (!cs2.perms.unseal) {
            except(ExceptionCause.PermitUnsealViolation, cs2Idx)
          } elsewhen (cs2Address < cs2.base) {
            except(ExceptionCause.LengthViolation, cs2Idx)
          } elsewhen (cs2Address >= cs2.top) {
            except(ExceptionCause.LengthViolation, cs2Idx)
          } otherwise {
            val cd = PackedCapability()
            cd.assignFrom(cs1)
            cd.otype.value.allowOverride
            cd.otype.unseal()

            output(context.data.CD_DATA).assignFrom(cd)
            output(pipeline.data.RD_VALID) := True
          }
        }
      }

      when (arbitration.isValid && value(Data.CINVOKE)) {
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        when (!arbitration.isStalled) {
          val target = cs1.address
          target.lsb := False

          when (!cs1.tag) {
            except(ExceptionCause.TagViolation, cs1Idx)
          } elsewhen (!cs2.tag) {
            except(ExceptionCause.TagViolation, cs2Idx)
          } elsewhen (!cs1.isSealed) {
            except(ExceptionCause.SealViolation, cs1Idx)
          } elsewhen (!cs2.isSealed) {
            except(ExceptionCause.SealViolation, cs2Idx)
          } elsewhen (cs1.otype.value =/= cs2.otype.value) {
            except(ExceptionCause.TypeViolation, cs1Idx)
          } elsewhen (!cs1.perms.cinvoke) {
            except(ExceptionCause.PermitCInvokeViolation, cs1Idx)
          } elsewhen (!cs2.perms.cinvoke) {
            except(ExceptionCause.PermitCInvokeViolation, cs2Idx)
          } elsewhen (!cs1.perms.execute) {
            except(ExceptionCause.PermitExecuteViolation, cs1Idx)
          } elsewhen (cs2.perms.execute) {
            except(ExceptionCause.PermitExecuteViolation, cs2Idx)
          } elsewhen (target < cs1.base) {
            except(ExceptionCause.LengthViolation, cs1Idx)
          } elsewhen (target >= cs1.top) {
            except(ExceptionCause.LengthViolation, cs1Idx)
          } otherwise {
            val targetPcc = PackedCapability()
            targetPcc.assignFrom(cs1)
            targetPcc.otype.value.allowOverride
            targetPcc.otype.unseal()
            pipeline.getService[PccService].jump(stage, targetPcc, cs1Idx)

            val cd = PackedCapability()
            cd.assignFrom(cs2)
            cd.otype.value.allowOverride
            cd.otype.unseal()
            output(context.data.CD_DATA).assignFrom(cd)
            output(pipeline.data.RD_VALID) := True
          }
        }
      }
    }
  }
}
