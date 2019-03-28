package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.Counter

class RiscvFormal(implicit config: Config) extends Plugin with FormalService {
  class Data(config: Config) {
    private val xlen = config.xlen

    object FORMAL_MEM_ADDR extends PipelineData(UInt(xlen bits))
    object FORMAL_MEM_RMASK extends PipelineData(Bits(xlen / 8 bits))
    object FORMAL_MEM_WMASK extends PipelineData(Bits(xlen / 8 bits))
    object FORMAL_MEM_RDATA extends PipelineData(UInt(xlen bits))
    object FORMAL_MEM_WDATA extends PipelineData(UInt(xlen bits))
    object FORMAL_MISALIGNED extends PipelineData(Bool())
  }

  private val data = new Data(config)

  override def lsuDefault(stage: Stage): Unit = {
    stage.output(data.FORMAL_MEM_ADDR) := 0
    stage.output(data.FORMAL_MEM_RMASK) := 0
    stage.output(data.FORMAL_MEM_WMASK) := 0
    stage.output(data.FORMAL_MEM_RDATA) := 0
    stage.output(data.FORMAL_MEM_WDATA) := 0
    stage.output(data.FORMAL_MISALIGNED) := False
  }

  override def lsuOnLoad(stage: Stage, addr: UInt,
                         rmask: Bits, rdata: UInt): Unit = {
    stage.output(data.FORMAL_MEM_ADDR) := addr
    stage.output(data.FORMAL_MEM_RMASK) := rmask
    stage.output(data.FORMAL_MEM_RDATA) := rdata
  }

  override def lsuOnStore(stage: Stage, addr: UInt,
                          wmask: Bits, wdata: UInt): Unit = {
    stage.output(data.FORMAL_MEM_ADDR) := addr
    stage.output(data.FORMAL_MEM_WDATA) := wdata
    stage.output(data.FORMAL_MEM_WMASK) := wmask
  }

  override def lsuOnMisaligned(stage: Stage): Unit = {
    stage.output(data.FORMAL_MISALIGNED) := True
  }

  override def build(pipeline: Pipeline): Unit = {
    class Rvfi extends Bundle {
      // Instruction Metadata
      val valid = Bool()
      val order = UInt(64 bits)
      val insn = UInt(32 bits)
      val trap = Bool()
      val halt = Bool()
      val intr = Bool()
      val mode = UInt(2 bits)

      // Integer Register Read/Write
      val rs1_addr,  rs2_addr,  rd_addr = UInt(5 bits)
      val rs1_rdata, rs2_rdata, rd_wdata = UInt(config.xlen bits)

      // Program Counter
      val pc_rdata, pc_wdata = UInt(config.xlen bits)

      // Memory Access
      val mem_addr = UInt(config.xlen bits)
      val mem_rmask, mem_wmask = Bits(config.xlen / 8 bits)
      val mem_rdata, mem_wdata = UInt(config.xlen bits)
    }

    val stage = pipeline.stages.last

    val rvfiArea = pipeline plug new Area {
      val rvfi = out(new Rvfi).keep()
      rvfi.valid := stage.arbitration.isDone
      rvfi.order := Counter(64 bits, rvfi.valid)
      rvfi.insn := stage.output(pipeline.data.IR)
      rvfi.trap := stage.output(pipeline.data.UNKNOWN_INSTRUCTION) ||
                   stage.output(data.FORMAL_MISALIGNED) ||
                   stage.output(pipeline.data.PC_MISALIGNED)
      rvfi.halt := False
      rvfi.mode := 3
      rvfi.rs1_addr := stage.output(pipeline.data.RS1)
      rvfi.rs2_addr := stage.output(pipeline.data.RS2)
      rvfi.rs1_rdata := (rvfi.rs1_addr === 0) ?
                        U(0) | stage.output(pipeline.data.RS1_DATA)
      rvfi.rs2_rdata := (rvfi.rs2_addr === 0) ?
                        U(0) | stage.output(pipeline.data.RS2_DATA)
      rvfi.rd_addr := stage.output(pipeline.data.RD)
      rvfi.rd_wdata := (rvfi.rd_addr === 0) ?
                       U(0) | stage.output(pipeline.data.RD_DATA)
      rvfi.pc_rdata := stage.output(pipeline.data.PC)
      rvfi.pc_wdata := stage.output(pipeline.data.NEXT_PC)
      rvfi.mem_addr := stage.output(data.FORMAL_MEM_ADDR)
      rvfi.mem_rmask := stage.output(data.FORMAL_MEM_RMASK)
      rvfi.mem_wmask := stage.output(data.FORMAL_MEM_WMASK)
      rvfi.mem_rdata := stage.output(data.FORMAL_MEM_RDATA)
      rvfi.mem_wdata := stage.output(data.FORMAL_MEM_WDATA)
    }

    rvfiArea.setName("")
  }
}
