package riscv.plugins.capabilities

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

class ScrFile(scrStage: Stage)(implicit context: Context) extends Plugin[Pipeline] with ScrService {
  private var ddcVar: RegCapability = _
  private var writingDdc: Bool = _

  private def ddc = {
    if (ddcVar == null) {
      scrStage plug new Area {
        val ddc = out(Reg(RegCapability()).init(RegCapability.Root))
        ddcVar = ddc
      }
    }

    ddcVar
  }

  object Data {
    object SCR_RW extends PipelineData(Bool())
  }

  private val scrs = mutable.Map[Int, Scr]()

  override def registerScr[T <: Scr](id: Int, scr: => T): T = {
    assert(!scrs.contains(id))
    val pluggesScr = scrStage.plug(scr)
    scrs(id) = pluggesScr
    pluggesScr
  }

  private class CapOffsetIo extends Bundle with IMasterSlave {
    val rdata = UInt(config.xlen bits)
    val wdata = UInt(config.xlen bits)
    val write = Bool()

    override def asMaster(): Unit = {
      in(rdata)
      out(wdata, write)
    }
  }

  override def registerScr[T <: Scr](id: Int, offsetCsr: Int, scr: => T): T = {
    val pluggedScr = registerScr(id, scr)
    val ioName = s"capOffset_$id"

    val scrArea = scrStage plug new Area {
      val io = slave(new CapOffsetIo).setPartialName(ioName)
      io.rdata := pluggedScr.read().offset

      when (io.write) {
        val newCap = PackedCapability()
        newCap.assignFrom(pluggedScr.read())
        newCap.offset.allowOverride
        newCap.offset := io.wdata
        pluggedScr.write(newCap)
      }
    }

    val csrArea = pipeline.getService[CsrService].registerCsr(offsetCsr, new Csr {
      val io = master(new CapOffsetIo).setPartialName(ioName)
      io.wdata.assignDontCare()
      io.write := False

      override def read(): UInt = {
        io.rdata
      }

      override def write(value: UInt): Unit = {
        io.wdata := value
        io.write := True
      }
    })

    pipeline plug {
      scrArea.io <> csrArea.io
    }

    pluggedScr
  }

  override def getScr(stage: Stage, id: Int): Scr = {
    assert(stage == scrStage)
    assert(scrs.contains(id))
    scrs(id)
  }

  override def getPcc(stage: Stage): Capability = {
    pipeline.getService[PccService].getPcc(stage)
  }

  override def getDdc(stage: Stage): Capability = {
    if (stage == scrStage) {
      ddc
    } else {
      val area = stage plug new Area {
        val ddc = in(RegCapability())
      }

      pipeline plug {
        area.ddc := ddc
      }

      area.ddc
    }
  }

  override def setup(): Unit = {
    val decoder = pipeline.getService[DecoderService]

    decoder.configure {config =>
      config.addDefault(Map(
        Data.SCR_RW -> False
      ))

      config.addDecoding(Opcodes.CSpecialRW, InstructionType.R_CxC, Map(
        Data.SCR_RW -> True
      ))
    }
  }

  override def build(): Unit = {
    scrStage plug new Area {
      import scrStage._

      val writingDdc = out(False)
      ScrFile.this.writingDdc = writingDdc

      val scrId = value(pipeline.data.RS2)
      val ignoreRead = value(pipeline.data.RD) === 0
      val ignoreWrite = value(pipeline.data.RS1) === 0
      val illegalScrId = False
      val pcc = getPcc(scrStage)
      val cs1 = value(context.data.CS1_DATA)
      val hasAsr = pcc.perms.accessSystemRegisters
      val needAsr = True
      val cd = RegCapability.Null

      when (arbitration.isValid && value(Data.SCR_RW)) {
        when(!ignoreRead) {
          switch (scrId) {
            is (ScrIndex.PCC) {
              cd.assignFrom(pcc)
              needAsr := False
            }
            is (ScrIndex.DDC) {
              cd.assignFrom(getDdc(scrStage))
              needAsr := False
            }

            for ((id, scr) <- scrs) {
              is (id) {
                cd.assignFrom(scr.read())
                needAsr := Bool(scr.needAsr)
              }
            }

            default {
              illegalScrId := True
            }
          }
        }

        when (!ignoreWrite) {
          switch (scrId) {
            is (ScrIndex.PCC) {
              needAsr := False
            }
            is (ScrIndex.DDC) {
              ddc := cs1
              writingDdc := True
            }

            for ((id, scr) <- scrs) {
              is (id) {
                scr.write(cs1)
                needAsr := Bool(scr.needAsr)
              }
            }

            default {
              illegalScrId := True
            }
          }
        }

        when (illegalScrId) {
          val trapHandler = pipeline.getService[TrapService]
          val ir = value(pipeline.data.IR)
          trapHandler.trap(scrStage, riscv.TrapCause.IllegalInstruction(ir))
        } elsewhen (needAsr && !hasAsr) {
          val exceptionHandler = pipeline.getService[ExceptionHandler]
          exceptionHandler.except(
            scrStage,
            ExceptionCause.AccessSystemRegistersViolation,
            CapIdx.scr(scrId)
          )
        } elsewhen (!ignoreRead) {
          output(context.data.CD_DATA) := cd
          output(pipeline.data.RD_VALID) := True
        }
      }
    }
  }

  override def finish(): Unit = {
    def writesDdc(stage: Stage) = {
      if (stage == scrStage) {
        writingDdc
      } else {
        stage.arbitration.isValid &&
          stage.input(Data.SCR_RW) &&
          stage.input(pipeline.data.RS2) === ScrIndex.DDC &&
          stage.input(pipeline.data.RS1) =/= 0
      }
    }

    val dbusStages = pipeline.getService[MemoryService].getDBusStages

    // To resolve hazards on DDC, we make an simplifying assumption that all
    // stages that access DBUS always need DDC. They will be stalled whenever
    // a later stage will write to DDC. This is an overestimation but since DDC
    // will not often be written to, this will probably not have a large
    // performance impact in practice.
    pipeline.getService[DataHazardService].resolveHazard({ (stage, nextStages) =>
      if (!dbusStages.contains(stage)) {
        False
      } else {
        nextStages.map(writesDdc).orR
      }
    })
  }
}
