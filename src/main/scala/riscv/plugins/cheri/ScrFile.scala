package riscv.plugins.cheri

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

class ScrFile(scrStage: Stage)(implicit context: Context) extends Plugin[Pipeline] with ScrService {
  private class ScrComponent extends Component {
    val io = master(ScrIo())

    // We treat DDC specially to make getDdc easier to implement. This should probably bd done using
    // registerScr like all other SCRs to make getScr also work for DDC.
    val ddc = out(Reg(RegCapability()).init(RegCapability.Root))
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

  object Data {
    object SCR_RW extends PipelineData(Bool())
  }

  private lazy val scrFile = pipeline plug new ScrComponent
  private val scrStageIos = mutable.Map[Stage, ScrIo]()
  private var writingDdc: Bool = _
  private val ddcStageInputs = mutable.Map[Stage, Capability]()
  private val scrs = mutable.Map[Int, Scr]()

  override def registerScr[T <: Scr](id: Int, scr: => T): T = {
    assert(!scrs.contains(id))
    val pluggedScr = scrFile.plug(scr)
    scrs(id) = pluggedScr
    pluggedScr
  }

  override def registerScr[T <: Scr](id: Int, offsetCsr: Int, scr: => T): T = {
    val pluggedScr = registerScr(id, scr)
    val ioName = s"capOffset_$id"

    val scrArea = scrFile plug new Area {
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
    assert(scrs.contains(id))

    // IO interface to directly access a single SCR.
    case class FixedScrIo() extends Bundle with IMasterSlave {
      val write = Bool()
      val wdata = PackedCapability()
      val rdata = PackedCapability()

      override def asMaster(): Unit = {
        in(write, wdata)
        out(rdata)
      }
    }

    val scr = scrs(id)
    val ioName = s"scr_$id"

    val scrArea = scrFile plug new Area {
      val io = master(FixedScrIo()).setPartialName(ioName)
      io.rdata.assignFrom(scr.read())

      when (io.write) {
        scr.write(io.wdata)
      }
    }

    val stageArea = stage plug new Area {
      val io = slave(FixedScrIo()).setPartialName(ioName)

      // To allow getScr to be called inside a conditional scope.
      Utils.outsideConditionScope {
        io.write := False
        io.wdata.assignDontCare()
      }
    }

    pipeline plug new Area {
      scrArea.io <> stageArea.io
    }

    new Scr {
      override val needAsr: Boolean = scr.needAsr
      override def read(): Capability = stageArea.io.rdata

      override def write(value: Capability): Unit = {
        stageArea.io.write := True
        stageArea.io.wdata.assignFrom(value)
      }
    }
  }

  override def getRegisteredScrs: Seq[Int] = scrs.keys.toSeq

  override def getIo(stage: Stage): ScrIo = {
    scrStageIos.getOrElseUpdate(stage, {
      val area = stage plug new Area {
        val scrIo = slave(ScrIo())
        scrIo.valid := False
        scrIo.hasAsr := False
        scrIo.id.assignDontCare()
        scrIo.write.assignDontCare()
        scrIo.wdata.assignDontCare()
      }

      area.scrIo
    })
  }

  override def getPcc(stage: Stage): Capability = {
    pipeline.getService[PccService].getPcc(stage)
  }

  override def getDdc(stage: Stage): Capability = {
    ddcStageInputs.getOrElseUpdate(stage, {
      val area = stage plug new Area {
        val ddc = in(RegCapability())
      }

      pipeline plug {
        area.ddc := scrFile.ddc
      }

      area.ddc
    })
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
    // Implementation of the ScrFile component.
    scrFile plug new Area {
      import scrFile._

      io.unknownId := False
      io.needAsr := False
      io.rdata.assignFrom(PackedCapability.Null)

      when (io.valid) {
        switch (io.id) {
          is (ScrIndex.DDC) {
            io.rdata.assignFrom(ddc)
            io.needAsr := False

            when(io.write) {
              ddc.assignFrom(io.wdata)
            }
          }

          for ((id, scr) <- scrs) {
            is (id) {
              io.needAsr := Bool(scr.needAsr)

              when (io.hasAsr || !io.needAsr) {
                io.rdata.assignFrom(scr.read())

                when (io.write) {
                  scr.write(io.wdata)
                }
              }
            }
          }

          default {
            io.unknownId := True
          }
        }
      }
    }

    // Implementation of the CSpecialRW instruction.
    scrStage plug new Area {
      import scrStage._

      val scrIo = getIo(scrStage)

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
        scrIo.valid := True
        scrIo.id := scrId
        scrIo.write := !ignoreWrite
        scrIo.wdata.assignFrom(cs1)
        scrIo.hasAsr := hasAsr

        illegalScrId := scrIo.unknownId
        cd.assignFrom(scrIo.rdata)
        needAsr := scrIo.needAsr

        // ScrFile doesn't store PCC (it's in pipeline registers).
        when (scrId === ScrIndex.PCC) {
          cd.assignFrom(pcc)
          needAsr := False
          illegalScrId := False
        } elsewhen (scrId === ScrIndex.DDC) {
          writingDdc := !ignoreWrite
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

    // Connect the ScrFile IO to all stages using it.
    pipeline plug new Area {
      scrFile.io.valid := False
      scrFile.io.id.assignDontCare()
      scrFile.io.write := False
      scrFile.io.wdata.assignDontCare()
      scrFile.io.hasAsr := False

      for (stageIo <- scrStageIos.values) {
        stageIo.rdata.assignDontCare()
        stageIo.unknownId := False
        stageIo.needAsr := False

        when (stageIo.valid) {
          scrFile.io <> stageIo
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
