package riscv

import riscv._
import spinal.core._
import spinal.lib._

case class RegisterFileReadIo()(implicit config: Config) extends Bundle with IMasterSlave {
  val rs1 = UInt(5 bits)
  val rs2 = UInt(5 bits)
  val rs1Data = UInt(config.xlen bits)
  val rs2Data = UInt(config.xlen bits)

  override def asMaster(): Unit = {
    in(rs1, rs2)
    out(rs1Data, rs2Data)
  }
}

case class RegisterFileWriteIo()(implicit config: Config) extends Bundle with IMasterSlave {
  val rd = UInt(5 bits)
  val data = UInt(config.xlen bits)
  val write = Bool()

  override def asMaster(): Unit = {
    in(rd, data, write)
  }
}

class RegisterFile()(implicit config: Config) extends Component {

  private val registerNames = Seq(
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0_fp", "s1", "a0", "a1",
    "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
  )

  val readIo = master(RegisterFileReadIo())
  val writeIo = master(RegisterFileWriteIo())
  val regs = Mem(UInt(config.xlen bits), config.numRegs)

  // Add a wire for each register with a readable name. This is to easily
  // view register values in a wave dump.
  for (i <- 0 until config.numRegs) {
    val regWire = UInt(config.xlen bits)
    val regName = registerNames(i)
    regWire.setName(s"x${i}_${regName}")
    regWire := regs.readAsync(U(i).resized, writeFirst)
  }

  // We explicitly check if x0 is read and return 0 in that case. Another
  // option would be to initialize regs(0) to zero at boot. However, this
  // slows-down riscv-formal runs significantly for some reason...
  def readReg(addr: UInt) = {
    addr.mux(
      0 -> U(0, config.xlen bits),
      default -> regs.readAsync(addr, writeFirst)
    )
  }

  readIo.rs1Data := readReg(readIo.rs1)
  readIo.rs2Data := readReg(readIo.rs2)

  when (writeIo.write && writeIo.rd =/= 0) {
    regs(writeIo.rd) := writeIo.data
  }
}