package riscv.plugins.cheri

import riscv._

import spinal.core._

class Lsu(stage: Stage)(implicit context: Context) extends Plugin[Pipeline] {
  object Data {
    object STORE_CAP extends PipelineData(Bool())
    object LOAD_CAP extends PipelineData(Bool())
    object USE_CAP_ADDR extends PipelineData(Bool())
  }

  private class CapCheck extends Area {
    private val cap = PackedCapability().assignDontCare()
    private val address = UInt(config.xlen bits).assignDontCare()
    private val operation = LsuOperationType().assignDontCare()
    private val byteWidth = UInt(log2Up(context.clen / 8) + 1 bits).assignDontCare()
    private val exceptionCause = UInt(5 bits)
    exceptionCause := ExceptionCause.None.code

    when(operation =/= LsuOperationType.NONE) {
      when(!cap.tag) {
        except(ExceptionCause.TagViolation)
      } elsewhen (cap.isSealed) {
        except(ExceptionCause.SealViolation)
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

    def check(
        cap: Capability,
        address: UInt,
        operation: SpinalEnumCraft[LsuOperationType.type],
        byteWidth: UInt
    ): UInt = {
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

    pipeline.service[LsuService] setAddressTranslator new LsuAddressTranslator {
      override def translate(
          stage: Stage,
          address: UInt,
          operation: SpinalEnumCraft[LsuOperationType.type],
          width: SpinalEnumCraft[LsuAccessWidth.type]
      ): UInt = {
        val result = UInt(config.xlen bits)
        result := address

        // When USE_CAP_ADDR is set, checks are done in build
        when(!stage.value(Data.USE_CAP_ADDR)) {
          val ddc = pipeline.service[ScrService].getDdc(stage)
          result := ddc.address + address

          val byteWidth = width.mux(
            LsuAccessWidth.B -> U(1),
            LsuAccessWidth.H -> U(2),
            LsuAccessWidth.W -> U(4)
          )

          when(stage.arbitration.isRunning) {
            val cause = capCheck.check(ddc, result, operation, byteWidth)

            when(cause =/= ExceptionCause.None.code) {
              val handler = pipeline.service[ExceptionHandler]
              handler.except(stage, cause, CapIdx.scr(ScrIndex.DDC))
            }
          }
        }

        result
      }
    }

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          Data.STORE_CAP -> False,
          Data.LOAD_CAP -> False,
          Data.USE_CAP_ADDR -> False
        )
      )

      config.addDecoding(
        Opcodes.SC_CAP,
        InstructionType.R_CCx,
        Map(
          Data.STORE_CAP -> True,
          Data.USE_CAP_ADDR -> True
        )
      )

      config.addDecoding(
        Opcodes.LC_CAP,
        InstructionType.R_CxC,
        Map(
          Data.LOAD_CAP -> True,
          Data.USE_CAP_ADDR -> True
        )
      )

      config.addDecoding(
        Opcodes.SC,
        InstructionType.S_RCx,
        Map(
          Data.STORE_CAP -> True
        )
      )

      config.addDecoding(
        Opcodes.LC,
        InstructionType.I_RxC,
        Map(
          Data.LOAD_CAP -> True
        )
      )

      val lsu = pipeline.service[LsuService]

      def addDataLoadCap(
          opcode: MaskedLiteral,
          width: SpinalEnumElement[LsuAccessWidth.type],
          unsigned: Boolean
      ) = {
        config.addDecoding(
          opcode,
          InstructionType.R_CxR,
          Map(
            Data.USE_CAP_ADDR -> True
          )
        )

        lsu.addLoad(opcode, width, unsigned)
      }

      addDataLoadCap(Opcodes.LB_CAP, LsuAccessWidth.B, unsigned = false)
      addDataLoadCap(Opcodes.LH_CAP, LsuAccessWidth.H, unsigned = false)
      addDataLoadCap(Opcodes.LW_CAP, LsuAccessWidth.W, unsigned = false)
      addDataLoadCap(Opcodes.LBU_CAP, LsuAccessWidth.B, unsigned = true)
      addDataLoadCap(Opcodes.LHU_CAP, LsuAccessWidth.H, unsigned = true)

      def addDataStoreCap(opcode: MaskedLiteral, width: SpinalEnumElement[LsuAccessWidth.type]) = {
        config.addDecoding(
          opcode,
          InstructionType.R_CRx,
          Map(
            Data.USE_CAP_ADDR -> True
          )
        )

        lsu.addStore(opcode, width)
      }

      addDataStoreCap(Opcodes.SB_CAP, LsuAccessWidth.B)
      addDataStoreCap(Opcodes.SH_CAP, LsuAccessWidth.H)
      addDataStoreCap(Opcodes.SW_CAP, LsuAccessWidth.W)
    }
  }

  override def build(): Unit = {
    stage plug new Area {
      import stage._

      val lsu = pipeline.service[LsuService]
      assert(stage == lsu.stage)

      when(arbitration.isValid && value(Data.USE_CAP_ADDR)) {
        arbitration.rs1Needed := True

        val cs1 = value(context.data.CS1_DATA)
        val address = cs1.address
        lsu.setAddress(address)

        // FIXME this code is duplicated from LsuAddressTranslator.translate above
        val byteWidth = lsu
          .width(stage)
          .mux(
            LsuAccessWidth.B -> U(1),
            LsuAccessWidth.H -> U(2),
            LsuAccessWidth.W -> U(4)
          )

        val cause = capCheck.check(cs1, address, lsu.operation(stage), byteWidth)

        when(!arbitration.isStalled) {
          when(cause =/= ExceptionCause.None.code) {
            val handler = pipeline.service[ExceptionHandler]
            handler.except(stage, cause, CapIdx.gpcr(value(pipeline.data.RS1)))
          }
        }
      }

      val cbus = pipeline.service[TaggedMemoryService].createCapBus(stage)
      cbus.cmd.valid := False
      cbus.cmd.payload.assignDontCare()
      cbus.rsp.ready := True

      val ddc = pipeline.service[ScrService].getDdc(stage)
      val cs1 = value(context.data.CS1_DATA)

      val ddcRelAddress = ddc.address + value(pipeline.data.RS1_DATA) + value(pipeline.data.IMM)
      val cs1Address = cs1.address

      val cap = PackedCapability()
      val capIdx = CapIdx()
      val address = UInt(config.xlen bits)

      when(value(Data.USE_CAP_ADDR)) {
        cap.assignFrom(cs1)
        capIdx := CapIdx.gpcr(value(pipeline.data.RS1))
        address := cs1Address
      } otherwise {
        cap.assignFrom(ddc)
        capIdx := CapIdx.scr(ScrIndex.DDC)
        address := ddcRelAddress
      }

      cbus.cmd.payload.address := address

      def checkCapBounds(operation: SpinalEnumCraft[LsuOperationType.type]): Bool = {
        val cause = capCheck.check(cap, address, operation, context.clen / 8)
        val ok = True

        when(cause =/= ExceptionCause.None.code) {
          val handler = pipeline.service[ExceptionHandler]
          handler.except(stage, cause, capIdx)
          ok := False
        }

        ok
      }

      when(arbitration.isValid && value(Data.STORE_CAP)) {
        arbitration.rs1Needed := True
        arbitration.rs2Needed := True

        when(!arbitration.isStalled) {
          when(checkCapBounds(LsuOperationType.STORE)) {
            val src = MemCapability()
            src.assignFrom(value(context.data.CS2_DATA))
            cbus.cmd.payload.wdata := src
            cbus.cmd.payload.write := True
            cbus.cmd.valid := True

            arbitration.isReady := cbus.cmd.ready
          }
        }
      }

      when(arbitration.isValid && value(Data.LOAD_CAP)) {
        arbitration.rs1Needed := True

        when(!arbitration.isStalled) {
          when(checkCapBounds(LsuOperationType.LOAD)) {
            cbus.cmd.payload.write := False
            cbus.cmd.valid := True
            arbitration.isReady := False

            when(cbus.rsp.valid) {
              output(context.data.CD_DATA).assignFrom(cbus.rsp.rdata)
              output(pipeline.data.RD_DATA_VALID) := True
              arbitration.isReady := True
            }
          }
        }
      }
    }
  }
}
