package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class Lsu(lsuStage: Stage)
         (implicit config: Config) extends Plugin with DBusService {
  object LsuAccessWidth extends SpinalEnum {
    val B, H, W = newElement()
  }

  object Data {
    object LSU_IS_LOAD extends PipelineData(Bool())
    object LSU_IS_STORE extends PipelineData(Bool())
    object LSU_ACCESS_WIDTH extends PipelineData(LsuAccessWidth())
    object LSU_IS_UNSIGNED extends PipelineData(Bool())
  }

  class DummyFormalService extends FormalService {
    override def lsuDefault(stage: Stage): Unit = ()
    override def lsuOnLoad(stage: Stage, addr: UInt,
                           rmask: Bits, rdata: UInt): Unit = ()
    override def lsuOnStore(stage: Stage, addr: UInt,
                            wmask: Bits, wdata: UInt): Unit = ()
    override def lsuOnMisaligned(stage: Stage): Unit = ()
  }

  private var dbus: MemBus = null

  override def getDBus: MemBus = {
    assert(dbus != null, "Call build() first")
    dbus
  }

  override def setup(pipeline: Pipeline): Unit = {
    val intAlu = pipeline.getService[IntAluService]

    val allOpcodes = Seq(Opcodes.LB, Opcodes.LH, Opcodes.LW, Opcodes.LBU,
      Opcodes.LHU, Opcodes.SB, Opcodes.SH, Opcodes.SW)

    for (opcode <- allOpcodes) {
      intAlu.addOperation(pipeline, opcode, intAlu.AluOp.ADD,
        intAlu.Src1Select.RS1, intAlu.Src2Select.IMM)
    }

    val decoder = pipeline.getService[DecoderService]

    decoder.configure(pipeline) {config =>
      config.addDefault(Map(
        Data.LSU_IS_LOAD -> False,
        Data.LSU_IS_STORE -> False,
        Data.LSU_IS_UNSIGNED -> False
      ))

      def addLoad(opcode: MaskedLiteral,
                  width: SpinalEnumElement[LsuAccessWidth.type],
                  unsigned: Bool) = {
        config.addDecoding(opcode, InstructionType.I, Map(
          Data.LSU_IS_LOAD -> True,
          Data.LSU_ACCESS_WIDTH -> width,
          Data.LSU_IS_UNSIGNED -> unsigned,
          pipeline.data.WRITE_RD -> True
        ))
      }

      addLoad(Opcodes.LW,  LsuAccessWidth.W, False)
      addLoad(Opcodes.LH,  LsuAccessWidth.H, False)
      addLoad(Opcodes.LB,  LsuAccessWidth.B, False)
      addLoad(Opcodes.LHU, LsuAccessWidth.H, True)
      addLoad(Opcodes.LBU, LsuAccessWidth.B, True)

      def addStore(opcode: MaskedLiteral,
                   width: SpinalEnumElement[LsuAccessWidth.type]) = {
        config.addDecoding(opcode, InstructionType.S, Map(
          Data.LSU_IS_STORE -> True,
          Data.LSU_ACCESS_WIDTH -> width
        ))
      }

      addStore(Opcodes.SW, LsuAccessWidth.W)
      addStore(Opcodes.SH, LsuAccessWidth.H)
      addStore(Opcodes.SB, LsuAccessWidth.B)
    }
  }

  override def build(pipeline: Pipeline): Unit = {
    val lsuArea = lsuStage plug new Area {
      import lsuStage._

      val dbus = master(new MemBus(config.dbusConfig))
      val dbusCtrl = new MemBusControl(dbus)

      val address = UInt(config.xlen bits)
      address := input(pipeline.getService[IntAluService].resultData)
      val busAddress = address & U(0xfffffffcL)

      val isLoad = value(Data.LSU_IS_LOAD)
      val isStore = value(Data.LSU_IS_STORE)
      val isActive = isLoad || isStore

      val misaligned = Bool()
      val baseMask = Bits(config.xlen / 8 bits)

      switch (value(Data.LSU_ACCESS_WIDTH)) {
        is (LsuAccessWidth.B) {
          misaligned := False
          baseMask := B"0001"
        }
        is (LsuAccessWidth.H) {
          misaligned := (address & 1) =/= 0
          baseMask := B"0011"
        }
        is (LsuAccessWidth.W) {
          misaligned := (address & 3) =/= 0
          baseMask := B"1111"
        }
      }

      val mask = baseMask |<< address(1 downto 0)

      val formal = if (pipeline.hasService[FormalService]) {
        pipeline.getService[FormalService]
      } else {
        new DummyFormalService
      }

      formal.lsuDefault(lsuStage)

      when (isActive && misaligned) {
        val trapHandler = pipeline.getService[TrapService]

        def trap(cause: TrapCause) = {
          trapHandler.trap(pipeline, lsuStage, cause)
        }

        when (isLoad) {
          trap(TrapCause.LoadAddressMisaligned(address))
        } otherwise {
          trap(TrapCause.StoreAddressMisaligned(address))
        }

        formal.lsuOnMisaligned(lsuStage)
      }

      when (arbitration.isValid && !misaligned) {
        when (isLoad) {
          val (valid, wValue) = dbusCtrl.read(busAddress)
          arbitration.isReady := valid
          val result = UInt(config.xlen bits)
          result := wValue

          switch (value(Data.LSU_ACCESS_WIDTH)) {
            is (LsuAccessWidth.H) {
              val offset = (address(1) ## B"0000").asUInt
              val hValue = wValue(offset, 16 bits)

              when (value(Data.LSU_IS_UNSIGNED)) {
                result := Utils.zeroExtend(hValue, config.xlen)
              } otherwise {
                result := Utils.signExtend(hValue, config.xlen)
              }
            }
            is (LsuAccessWidth.B) {
              val offset = (address(1 downto 0) ## B"000").asUInt
              val bValue = wValue(offset, 8 bits)

              when (value(Data.LSU_IS_UNSIGNED)) {
                result := Utils.zeroExtend(bValue, config.xlen)
              } otherwise {
                result := Utils.signExtend(bValue, config.xlen)
              }
            }
          }

          output(pipeline.data.RD_DATA) := result
          output(pipeline.data.RD_VALID) := True

          formal.lsuOnLoad(lsuStage, busAddress, mask, wValue)
        }

        when (isStore) {
          val wValue = value(pipeline.data.RS2_DATA)
          arbitration.rs2Needed := True
          val data = UInt(config.xlen bits)
          data := wValue

          switch (value(Data.LSU_ACCESS_WIDTH)) {
            is (LsuAccessWidth.H) {
              val hValue = wValue(15 downto 0)

              when (address(1)) {
                data := hValue << 16
              } otherwise {
                data := hValue.resized
              }
            }
            is (LsuAccessWidth.B) {
              val bValue = wValue(7 downto 0)

              switch (address(1 downto 0).asBits) {
                is (B"00") {
                  data := bValue.resized
                }
                is (B"01") {
                  data := (bValue << 8).resized
                }
                is (B"10") {
                  data := (bValue << 16).resized
                }
                is (B"11") {
                  data := (bValue << 24).resized
                }
              }
            }
          }

          val accepted = dbusCtrl.write(busAddress, data, mask)
          arbitration.isReady := accepted

          formal.lsuOnStore(lsuStage, busAddress, mask, data)
        }
      }
    }

    val pipelineArea = pipeline plug new Area {
      val dbus = master(new MemBus(config.dbusConfig))
      dbus <> lsuArea.dbus
      Lsu.this.dbus = dbus
    }

    pipelineArea.setName("")
  }
}
