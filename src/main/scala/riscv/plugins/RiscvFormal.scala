package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.Counter

class RiscvFormal(altops: Boolean = false) extends Plugin[Pipeline] with FormalService {
  private object Data {
    private val xlen = config.xlen

    object FORMAL_MEM_ADDR extends PipelineData(UInt(xlen bits))
    object FORMAL_MEM_RMASK extends PipelineData(Bits(xlen / 8 bits))
    object FORMAL_MEM_WMASK extends PipelineData(Bits(xlen / 8 bits))
    object FORMAL_MEM_RDATA extends PipelineData(UInt(xlen bits))
    object FORMAL_MEM_WDATA extends PipelineData(UInt(xlen bits))
    object FORMAL_MISALIGNED extends PipelineData(Bool())
  }

  class Rvfi extends Bundle {
    // Instruction Metadata
    val valid = Bool()
    val order = UInt(64 bits)
    val insn = UInt(32 bits)
    val trap = Bool()
    val halt = Bool()
    val intr = Bool()
    val mode = UInt(2 bits)
    val ixl = UInt(2 bits)

    // Integer Register Read/Write
    val rs1_addr, rs2_addr, rd_addr = UInt(5 bits)
    val rs1_rdata, rs2_rdata, rd_wdata = UInt(config.xlen bits)

    // Program Counter
    val pc_rdata, pc_wdata = UInt(config.xlen bits)

    // Memory Access
    val mem_addr = UInt(config.xlen bits)
    val mem_rmask, mem_wmask = Bits(config.xlen / 8 bits)
    val mem_rdata, mem_wdata = UInt(config.xlen bits)

    // Internal signals
    // Has this instruction experienced an exception or interrupt?
    val hasTrapped = Bool()
  }

  override def lsuDefault(stage: Stage): Unit = {
    stage.output(Data.FORMAL_MEM_ADDR) := 0
    stage.output(Data.FORMAL_MEM_RMASK) := 0
    stage.output(Data.FORMAL_MEM_WMASK) := 0
    stage.output(Data.FORMAL_MEM_RDATA) := 0
    stage.output(Data.FORMAL_MEM_WDATA) := 0
    stage.output(Data.FORMAL_MISALIGNED) := False
  }

  override def lsuOnLoad(stage: Stage, addr: UInt, rmask: Bits, rdata: UInt): Unit = {
    stage.output(Data.FORMAL_MEM_ADDR) := addr
    stage.output(Data.FORMAL_MEM_RMASK) := rmask
    stage.output(Data.FORMAL_MEM_RDATA) := rdata
  }

  override def lsuOnStore(stage: Stage, addr: UInt, wmask: Bits, wdata: UInt): Unit = {
    stage.output(Data.FORMAL_MEM_ADDR) := addr
    stage.output(Data.FORMAL_MEM_WDATA) := wdata
    stage.output(Data.FORMAL_MEM_WMASK) := wmask
  }

  override def lsuOnMisaligned(stage: Stage): Unit = {
    stage.output(Data.FORMAL_MISALIGNED) := True
  }

  override def build(): Unit = {
    val stage = pipeline.retirementStage
    val trapService = pipeline.service[TrapService]

    def zeroIfNone(
        regType: PipelineData[SpinalEnumCraft[RegisterType.type]],
        data: PipelineData[UInt]
    ): UInt = {
      (stage.output(regType) === RegisterType.NONE) ? U(0) | stage.output(data)
    }

    val rvfiArea = pipeline plug new Area {
      // RVFI data for the instruction that is currently being retired. This
      // data is *not* used as the output because it is impossible to know the
      // next PC (pc_wdata) reliably. (More specifically, before we used the
      // NEXT_PC pipeline register but this hid a bug where an instruction
      // further down the pipeline disappeared while all previous instructions
      // *did* have a correct NEXT_PC.) Therefore, we buffer one instruction in
      // prevRvfi and use that data as output combined with the PC of
      // currentRvfi as the next PC. This ensures that pc_wdata *really*
      // reflects the PC of the next instruction.
      val currentRvfi = new Rvfi
      currentRvfi.valid := stage.arbitration.isDone
      currentRvfi.order.assignDontCare()
      currentRvfi.insn := stage.output(pipeline.data.IR)
      currentRvfi.trap := trapService.hasException(stage)
      currentRvfi.halt := False
      currentRvfi.mode := 3
      currentRvfi.ixl := 1
      currentRvfi.rs1_addr := zeroIfNone(pipeline.data.RS1_TYPE, pipeline.data.RS1)
      currentRvfi.rs2_addr := zeroIfNone(pipeline.data.RS2_TYPE, pipeline.data.RS2)
      currentRvfi.rs1_rdata := zeroIfNone(pipeline.data.RS1_TYPE, pipeline.data.RS1_DATA)
      currentRvfi.rs2_rdata := zeroIfNone(pipeline.data.RS2_TYPE, pipeline.data.RS2_DATA)
      // FIXME This will hide bugs that still write RD on traps
      currentRvfi.rd_addr :=
        (currentRvfi.trap || stage.output(pipeline.data.RD_TYPE) === RegisterType.NONE) ?
          U(0) | stage.output(pipeline.data.RD)
      currentRvfi.rd_wdata := (currentRvfi.rd_addr === 0) ?
        U(0) | stage.output(pipeline.data.RD_DATA)
      currentRvfi.pc_rdata := stage.output(pipeline.data.PC)
      currentRvfi.mem_addr := stage.output(Data.FORMAL_MEM_ADDR)
      currentRvfi.mem_rmask := stage.output(Data.FORMAL_MEM_RMASK)
      currentRvfi.mem_wmask := stage.output(Data.FORMAL_MEM_WMASK)
      currentRvfi.mem_rdata := stage.output(Data.FORMAL_MEM_RDATA)
      currentRvfi.mem_wdata := stage.output(Data.FORMAL_MEM_WDATA)
      currentRvfi.hasTrapped := trapService.hasTrapped(stage)

      if (altops) {
        implementAltops(currentRvfi)
      }

      val prevRvfi = Reg(new Rvfi).init({
        val init = new Rvfi
        init.assignDontCare()
        init.valid.allowOverride
        init.valid := False
        init
      })

      when(currentRvfi.valid) {
        prevRvfi := currentRvfi
      }

      val rvfi = out(new Rvfi).keep()
      rvfi := prevRvfi
      rvfi.pc_wdata.allowOverride
      rvfi.pc_wdata := currentRvfi.pc_rdata
      rvfi.valid.allowOverride
      // Only send out instructions that have not trapped once a next
      // instruction has arrived
      rvfi.valid := currentRvfi.valid && prevRvfi.valid // && !prevRvfi.hasTrapped
      rvfi.order.allowOverride
      rvfi.order := Counter(64 bits, rvfi.valid)
    }

    rvfiArea.setName("")
  }

  private def implementAltops(rvfi: Rvfi) = {
    val altopsTable = Seq[(MaskedLiteral, (UInt, UInt) => UInt, BigInt)](
      (Opcodes.MUL, _ + _, 0x2cdf52a55876063eL),
      (Opcodes.MULH, _ + _, 0x15d01651f6583fb7L),
      (Opcodes.MULHSU, _ - _, 0xea3969edecfbe137L),
      (Opcodes.MULHU, _ + _, 0xd13db50d949ce5e8L),
      (Opcodes.DIV, _ - _, 0x29bbf66f7f8529ecL),
      (Opcodes.DIVU, _ - _, 0x8c629acb10e8fd70L),
      (Opcodes.REM, _ - _, 0xf5b7d8538da68fa5L),
      (Opcodes.REMU, _ - _, 0xbc4402413138d0e1L)
    )

    switch(rvfi.insn) {
      for ((opcode, altop, fullMask) <- altopsTable) {
        is(opcode) {
          val mask = if (config.xlen == 32) fullMask & 0xffffffffL else fullMask
          when(rvfi.rd_addr =/= 0) {
            rvfi.rd_wdata := altop(rvfi.rs1_rdata, rvfi.rs2_rdata) ^ mask
          }
        }
      }
    }
  }
}
