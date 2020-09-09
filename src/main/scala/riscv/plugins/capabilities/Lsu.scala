package riscv.plugins.capabilities

import riscv._

import spinal.core._

class Lsu(stage: Stage)(implicit context: Context) extends Plugin[Pipeline] {
  object Data {
    object STORE_CAP extends PipelineData(Bool())
    object LOAD_CAP extends PipelineData(Bool())
  }

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

    pipeline.getService[DecoderService].configure {config =>
      config.addDefault(Map(
        Data.STORE_CAP -> False,
        Data.LOAD_CAP -> False
      ))

      config.addDecoding(Opcodes.SC, InstructionType.S_RCx, Map(
        Data.STORE_CAP -> True
      ))

      config.addDecoding(Opcodes.LC, InstructionType.I_RxC, Map(
        Data.LOAD_CAP -> True
      ))
    }
  }

  override def build(): Unit = {
    stage plug new Area {
      import stage._

      val cbus = pipeline.getService[TaggedMemoryService].createCapBus(stage)
      cbus.cmd.valid := False
      cbus.cmd.payload.assignDontCare()
      cbus.rsp.ready := True

      val address = value(pipeline.data.RS1_DATA) + value(pipeline.data.IMM)
      cbus.cmd.payload.address := address

      when (arbitration.isValid && value(Data.STORE_CAP)) {
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        when (!arbitration.isStalled) {
          val src = MemCapability()
          src.assignFrom(value(context.data.CS2_DATA))
          cbus.cmd.payload.wdata := src
          cbus.cmd.payload.write := True
          cbus.cmd.valid := True

          arbitration.isReady := cbus.cmd.ready
        }
      }

      when (arbitration.isValid && value(Data.LOAD_CAP)) {
        arbitration.rs1Needed := True

        when (!arbitration.isStalled) {
          cbus.cmd.payload.write := False
          cbus.cmd.valid := True
          arbitration.isReady := False

          when (cbus.rsp.valid) {
            output(context.data.CD_DATA).assignFrom(cbus.rsp.rdata)
            output(pipeline.data.RD_VALID) := True
            arbitration.isReady := True
          }
        }
      }
    }
  }
}
