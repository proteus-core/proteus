package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

private class CsrIo(implicit config: Config) extends Bundle with IMasterSlave {
  val rid, wid = UInt(12 bits)
  val rdata, wdata = UInt(config.xlen bits)
  val read, write = Bool()
  val error = Bool()

  override def asMaster(): Unit = {
    in(rid, wid, wdata, read, write)
    out(rdata, error)
  }

  def read(id: UInt): UInt = {
    rid := id
    read := True
    rdata
  }

  def write(id: UInt, data: UInt): Unit = {
    wid := id
    write := True
    wdata := data
  }
}

private class CsrComponent(implicit config: Config) extends Component {
  setDefinitionName("CsrFile")

  val io = master(new CsrIo)
}

class CsrFile(implicit config: Config) extends Plugin with CsrService {
  object Opcodes {
    val CSRRW  = M"-----------------001-----1110011"
    val CSRRS  = M"-----------------010-----1110011"
    val CSRRC  = M"-----------------011-----1110011"
    val CSRRWI = M"-----------------101-----1110011"
    val CSRRSI = M"-----------------110-----1110011"
    val CSRRCI = M"-----------------111-----1110011"
  }

  object CsrOp extends SpinalEnum {
    val NONE, RW, RS, RC = newElement()
  }

  object Data {
    object CSR_OP extends PipelineData(CsrOp())
    object CSR_USE_IMM extends PipelineData(Bool())
  }

  private var component: CsrComponent = null
  private val registers = mutable.Map[Int, Register]()

  private def component(pipeline: Pipeline): CsrComponent = {
    if (component == null) {
      component = pipeline plug new CsrComponent
    }

    component
  }

  private def isReadOnly(csrId: Int) = (csrId & 0xC00) == 0xC00

  override def registerCsr[T <: Register](pipeline: Pipeline, id: Int, reg: => T): T = {
    assert(id >= 0 && id < 4096, "Illegal CSR id")
    assert(!registers.contains(id), s"Multiple CSRs with id $id")

    val pluggedReg = component(pipeline).plug(reg)
    registers(id) = pluggedReg
    pluggedReg
  }

  override def setup(pipeline: Pipeline): Unit = {
    val decoder = pipeline.getService[DecoderService]

    decoder.configure(pipeline) {config =>
      config.addDefault(Map(
        Data.CSR_OP -> CsrOp.NONE,
        Data.CSR_USE_IMM -> False
      ))

      val ops = Seq(
        (Opcodes.CSRRW,  CsrOp.RW, False),
        (Opcodes.CSRRS,  CsrOp.RS, False),
        (Opcodes.CSRRC,  CsrOp.RC, False),
        (Opcodes.CSRRWI, CsrOp.RW, True),
        (Opcodes.CSRRSI, CsrOp.RS, True),
        (Opcodes.CSRRCI, CsrOp.RC, True)
      )

      for ((opcode, op, useImm) <- ops) {
        config.addDecoding(opcode, InstructionType.I, Map(
          Data.CSR_OP -> op,
          Data.CSR_USE_IMM -> useImm,
          pipeline.data.WRITE_RD -> True
        ))
      }
    }
  }

  override def build(pipeline: Pipeline): Unit = {
    val csrComponent = component(pipeline)

    csrComponent plug new Area {
      import csrComponent._

      io.rdata := 0
      io.error := False

      when (io.read) {
        switch (io.rid) {
          for ((id, reg) <- registers) {
            is (id) {
              io.rdata := reg.read()
            }
          }
          default {
            io.error := True
          }
        }
      }

      when (io.write) {
        switch (io.wid) {
          for ((id, reg) <- registers) {
            is (id) {
              if (isReadOnly(id)) {
                io.error := True
              } else {
                reg.write(io.wdata)
              }
            }
          }
          default {
            io.error := True
          }
        }
      }
    }

    val csrStage = pipeline.writeback

    val csrArea = csrStage plug new Area {
      import csrStage._

      val csrIo = slave(new CsrIo)
      csrIo.rid := 0
      csrIo.wid := 0
      csrIo.read := False
      csrIo.write := False
      csrIo.wdata := 0

      val csrId = value(pipeline.data.IR)(31 downto 20)
      val op = value(Data.CSR_OP)
      val ignoreRead = value(pipeline.data.RD) === 0
      val ignoreWrite = False
      val src = UInt(config.xlen bits)

      when (value(Data.CSR_USE_IMM)) {
        src := DataTools.zeroExtend(value(pipeline.data.RS1), config.xlen)
      } otherwise {
        src := value(pipeline.data.RS1_DATA)
        arbitration.rs1Needed := True
        ignoreWrite := value(pipeline.data.RS1) === 0
      }

      def outputRd(value: UInt) = {
        output(pipeline.data.RD_DATA) := value
        output(pipeline.data.RD_VALID) := True
      }

      when (arbitration.isValid && op =/= CsrOp.NONE) {
        switch (op) {
          is(CsrOp.RW) {
            when(!ignoreRead) {
              outputRd(csrIo.read(csrId))
            }

            csrIo.write(csrId, src)
          }
          is (CsrOp.RS) {
            val oldCsrVal = csrIo.read(csrId)
            outputRd(oldCsrVal)
            val newCsrVal = oldCsrVal | src

            when (!ignoreWrite) {
              csrIo.write(csrId, newCsrVal)
            }
          }
          is (CsrOp.RC) {
            val oldCsrVal = csrIo.read(csrId)
            outputRd(oldCsrVal)
            val newCsrVal = oldCsrVal & ~src

            when (!ignoreWrite) {
              csrIo.write(csrId, newCsrVal)
            }
          }
        }

        when (csrIo.error) {
          output(pipeline.data.UNKNOWN_INSTRUCTION) := True
        }
      }
    }

    pipeline plug new Area {
      csrComponent.io <> csrArea.csrIo
    }
  }
}
