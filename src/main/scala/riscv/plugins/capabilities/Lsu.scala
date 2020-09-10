package riscv.plugins.capabilities

import riscv._

import spinal.core._

class Lsu(stage: Stage)(implicit context: Context) extends Plugin[Pipeline] {
  object Data {
    object STORE_CAP extends PipelineData(Bool())
    object LOAD_CAP extends PipelineData(Bool())
  }

  private class CapCheck extends Area {
    private val cap = PackedCapability().assignDontCare()
    private val address = UInt(config.xlen bits).assignDontCare()
    private val operation = LsuOperationType()
    operation := LsuOperationType.NONE
    private val byteWidth = UInt(log2Up(context.clen / 8) + 1 bits).assignDontCare()
    private val exceptionCause = UInt(5 bits)
    exceptionCause := ExceptionCause.None.code

    when (operation =/= LsuOperationType.NONE) {
      when (!cap.tag) {
        except(ExceptionCause.TagViolation)
      } elsewhen (operation === LsuOperationType.LOAD && !cap.perms.load) {
        except(ExceptionCause.PermitLoadViolation)
      } elsewhen (operation === LsuOperationType.STORE && !cap.perms.store) {
        except(ExceptionCause.PermitStoreViolation)
      } elsewhen (address + byteWidth > cap.top) {
        except(ExceptionCause.LengthViolation)
      } elsewhen (address < cap.base) {
        except(ExceptionCause.LengthViolation)
      }
    }

    private def except(cause: ExceptionCause) = {
      exceptionCause := cause.code
    }

    def check(cap: Capability, address: UInt,
              operation: SpinalEnumCraft[LsuOperationType.type],
              byteWidth: UInt): UInt = {
      assert(byteWidth.getBitsWidth <= this.byteWidth.getBitsWidth)

      this.cap.assignFrom(cap)
      this.address := address
      this.operation := operation
      this.byteWidth := byteWidth.resized
      exceptionCause
    }
  }

  private var capCheck: CapCheck = _

  override def setup(): Unit = {
    stage plug new Area {
      val capCheck = new CapCheck
      Lsu.this.capCheck = capCheck
    }

    pipeline.getService[LsuService] setAddressTranslator new LsuAddressTranslator {
      override def translate(stage: Stage,
                             address: UInt,
                             operation: SpinalEnumCraft[LsuOperationType.type],
                             width: SpinalEnumCraft[LsuAccessWidth.type]): UInt = {
        val ddc = pipeline.getService[ScrService].getDdc(stage)
        val result = ddc.address + address

        val byteWidth = width.mux(
          LsuAccessWidth.B -> U(1),
          LsuAccessWidth.H -> U(2),
          LsuAccessWidth.W -> U(4)
        )

        when (stage.arbitration.isRunning) {
          val cause = capCheck.check(ddc, result, operation, byteWidth)

          when (cause =/= ExceptionCause.None.code) {
            val handler = pipeline.getService[ExceptionHandler]
            handler.except(stage, cause, ScrIndex.DDC)
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

      val ddc = pipeline.getService[ScrService].getDdc(stage)
      val address = ddc.address + value(pipeline.data.RS1_DATA) + value(pipeline.data.IMM)
      cbus.cmd.payload.address := address

      def checkCapBounds(operation: SpinalEnumCraft[LsuOperationType.type]): Bool = {
        val cause = capCheck.check(ddc, address, operation, context.clen / 8)
        val ok = True

        when (cause =/= ExceptionCause.None.code) {
          val handler = pipeline.getService[ExceptionHandler]
          handler.except(stage, cause, ScrIndex.DDC)
          ok := False
        }

        ok
      }

      when (arbitration.isValid && value(Data.STORE_CAP)) {
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        when (!arbitration.isStalled) {
          when (checkCapBounds(LsuOperationType.STORE)) {
            val src = MemCapability()
            src.assignFrom(value(context.data.CS2_DATA))
            cbus.cmd.payload.wdata := src
            cbus.cmd.payload.write := True
            cbus.cmd.valid := True

            arbitration.isReady := cbus.cmd.ready
          }
        }
      }

      when (arbitration.isValid && value(Data.LOAD_CAP)) {
        arbitration.rs1Needed := True

        when (!arbitration.isStalled) {
          when (checkCapBounds(LsuOperationType.LOAD)) {
            cbus.cmd.payload.write := False
            cbus.cmd.valid := True
            arbitration.isReady := False

            when(cbus.rsp.valid) {
              output(context.data.CD_DATA).assignFrom(cbus.rsp.rdata)
              output(pipeline.data.RD_VALID) := True
              arbitration.isReady := True
            }
          }
        }
      }
    }
  }
}
