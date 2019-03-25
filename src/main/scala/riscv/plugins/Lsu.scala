package riscv.plugins

import riscv._

import spinal.core._

class Lsu extends Plugin {
  object Opcodes {
    val LB  = M"-----------------000-----0000011"
    val LH  = M"-----------------001-----0000011"
    val LW  = M"-----------------010-----0000011"
    val LBU = M"-----------------100-----0000011"
    val LHU = M"-----------------101-----0000011"
    val SB  = M"-----------------000-----0100011"
    val SH  = M"-----------------001-----0100011"
    val SW  = M"-----------------010-----0100011"
  }

  object LsuAccessWidth extends SpinalEnum {
    val B, H, W = newElement()
  }

  object Data {
    object LSU_IS_LOAD extends PipelineData(Bool())
    object LSU_IS_STORE extends PipelineData(Bool())
    object LSU_ACCESS_WIDTH extends PipelineData(LsuAccessWidth())
    object LSU_IS_UNSIGNED extends PipelineData(Bool())
  }

  override def setup(pipeline: Pipeline, config: Config): Unit = {
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

  override def build(pipeline: Pipeline, config: Config): Unit = {
    pipeline.memory plug new Area {
      import pipeline.memory._

      var i: BigInt = 0
      val dmem = Mem(UInt(config.xlen bits), 1024).init(Seq.fill(1024) {
        def wrap(i: BigInt) = i % 256
        val content = wrap(i) + (wrap(i + 1) << 8) + (wrap(i + 2) << 16) + (wrap(i + 3) << 24)
        val value = U(content).setWidth(config.xlen)
        i += 4
        value
      })

      val address = UInt(config.xlen bits)
      address := input(pipeline.data.RD_DATA)
      val memAddress = address >> 2

      val misaligned = False

      switch (value(Data.LSU_ACCESS_WIDTH)) {
        is (LsuAccessWidth.H) {
          misaligned := (address & 1) =/= 0
        }
        is (LsuAccessWidth.W) {
          misaligned := (address & 3) =/= 0
        }
      }

      val result = UInt(config.xlen bits)
      result := 0

      when (!misaligned) {
        when (value(Data.LSU_IS_LOAD)) {
          val wValue = dmem(memAddress.resized)
          result := wValue

          switch (value(Data.LSU_ACCESS_WIDTH)) {
            is (LsuAccessWidth.H) {
              val offset = (address(1) ## B"0000").asUInt
              val hValue = wValue(offset, 16 bits)

              when (value(Data.LSU_IS_UNSIGNED)) {
                result := DataTools.zeroExtend(hValue, config.xlen)
              } otherwise {
                result := DataTools.signExtend(hValue, config.xlen)
              }
            }
            is (LsuAccessWidth.B) {
              val offset = (address(1 downto 0) ## B"000").asUInt
              val bValue = wValue(offset, 8 bits)

              when (value(Data.LSU_IS_UNSIGNED)) {
                result := DataTools.zeroExtend(bValue, config.xlen)
              } otherwise {
                result := DataTools.signExtend(bValue, config.xlen)
              }
            }
          }

          output(pipeline.data.RD_DATA) := result
          output(pipeline.data.RD_VALID) := True
        }

        when (value(Data.LSU_IS_STORE)) {
          val wValue = value(pipeline.data.RS2_DATA)
          val data = UInt(config.xlen bits)
          data := wValue
          val mask = Bits(4 bits)
          mask := B"1111"

          switch (value(Data.LSU_ACCESS_WIDTH)) {
            is (LsuAccessWidth.H) {
              val hValue = wValue(15 downto 0)

              when (address(1)) {
                data := hValue << 16
                mask := B"1100"
              } otherwise {
                data := hValue.resized
                mask := B"0011"
              }
            }
            is (LsuAccessWidth.B) {
              val bValue = wValue(7 downto 0)

              switch (address(1 downto 0).asBits) {
                is (B"00") {
                  data := bValue.resized
                  mask := B"0001"
                }
                is (B"01") {
                  data := (bValue << 8).resized
                  mask := B"0010"
                }
                is (B"10") {
                  data := (bValue << 16).resized
                  mask := B"0100"
                }
                is (B"11") {
                  data := (bValue << 24).resized
                  mask := B"1000"
                }
              }
            }
          }

          dmem.write(memAddress.resized, data, mask = mask)
        }
      }
    }
  }
}
