package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

private class CsrFileIo(implicit config: Config) extends Bundle with IMasterSlave {
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

private case class CsrWriteNotify() extends Bundle with IMasterSlave {
  val write = Bool()

  override def asMaster(): Unit = {
    out(write)
  }
}

private class CsrComponent(implicit config: Config) extends Component {
  setDefinitionName("CsrFile")

  val io = master(new CsrFileIo)
  val writeNotify = master(CsrWriteNotify())
}

class CsrFile(csrStage: Stage, exeStage: Stage) extends Plugin[Pipeline] with CsrService {
  object CsrOp extends SpinalEnum {
    val NONE, RW, RS, RC = newElement()
  }

  object Data {
    object CSR_OP extends PipelineData(CsrOp())
    object CSR_USE_IMM extends PipelineData(Bool())
  }

  // lazy because pipeline is null at the time of construction.
  private lazy val component = pipeline plug new CsrComponent
  private val registers = mutable.Map[Int, Csr]()
  private var writeInCycle: Bool = null

  private def isReadOnly(csrId: Int) = (csrId & 0xC00) == 0xC00

  override def registerCsr[T <: Csr](id: Int, reg: => T): T = {
    assert(id >= 0 && id < 4096, "Illegal CSR id")
    assert(!registers.contains(id), s"Multiple CSRs with id $id")

    val pluggedReg = component.plug(reg)
    registers(id) = pluggedReg
    pluggedReg
  }

  override def getCsr(id: Int): CsrIo = {
    assert(registers.contains(id))

    val area = component plug new Area {
      val csrIo = master(new CsrIo())
      csrIo.setName(s"io_$id")
      val reg = registers(id)

      csrIo.rdata := reg.read()

      when (csrIo.write) {
        reg.write(csrIo.wdata)
      }
    }

    area.csrIo
  }

  override def setup(): Unit = {
    val decoder = pipeline.getService[DecoderService]
    val issuer = pipeline.getService[IssueService]

    decoder.configure {config =>
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
          Data.CSR_USE_IMM -> useImm
        ))
        issuer.setDestination(opcode, exeStage)
      }
    }
  }

  override def build(): Unit = {
    val csrComponent = component

    exeStage plug new Area { // TODO: hack
      exeStage.value(Data.CSR_OP)
      exeStage.value(Data.CSR_USE_IMM)
      exeStage.value(pipeline.data.RS1)
    }

    csrComponent plug new Area {
      import csrComponent._

      io.rdata := 0
      io.error := False

      writeNotify.write := False

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
                reg.swWrite(io.wdata)
                writeNotify.write := True
              }
            }
          }
          default {
            io.error := True
          }
        }
      }
    }

    val csrArea = csrStage plug new Area {
      import csrStage._

      val csrIo = slave(new CsrFileIo)
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
        src := Utils.zeroExtend(value(pipeline.data.RS1), config.xlen)
      } otherwise {
        src := value(pipeline.data.RS1_DATA)
        arbitration.rs1Needed := True
        ignoreWrite := value(pipeline.data.RS1) === 0
      }

      def outputRd(value: UInt) = {
        output(pipeline.data.RD_DATA) := value
        output(pipeline.data.RD_DATA_VALID) := True
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
          val trapHandler = pipeline.getService[TrapService]
          trapHandler.trap(csrStage, TrapCause.IllegalInstruction(value(pipeline.data.IR)))
        }
      }
    }

    pipeline plug new Area {
      csrComponent.io <> csrArea.csrIo
    }
  }

  override def isCsrInstruction(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.element(Data.CSR_OP.asInstanceOf[PipelineData[Data]]) =/= CsrOp.NONE
  }

  override def csrWriteInCycle(): Bool = {
    component.writeNotify.write
  }
}
