package riscv.plugins.capabilities

import riscv._
import spinal.core._
import spinal.lib._

class ScrFile(scrStage: Stage)(implicit context: Context) extends Plugin[Pipeline] with ScrService {
  private var ddcVar: Capability = _
  private var writingDdc: Bool = _

  private def ddc = {
    if (ddcVar == null) {
      scrStage plug new Area {
        val ddc = out(Reg(Capability()).init(Capability.Root))
        ddcVar = ddc
      }
    }

    ddcVar
  }

  object Data {
    object SCR_RW extends PipelineData(Bool())
  }

  override def getPcc(stage: Stage): Capability = {
    val pcc = Capability()
    pcc.assignFrom(stage.value(context.data.PCC), stage.value(pipeline.data.PC))
    pcc
  }

  override def getDdc(stage: Stage): Capability = {
    if (stage == scrStage) {
      ddc
    } else {
      val area = stage plug new Area {
        val ddc = in(Capability())
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

      pipeline plug {
        // FIXME: This should probably be done by a "PccManager" plugin
        pipeline.fetchStage.input(context.data.PCC) := Capability.Root
      }

      val writingDdc = out(False)
      ScrFile.this.writingDdc = writingDdc

      val scrId = value(pipeline.data.RS2)
      val ignoreRead = value(pipeline.data.RD) === 0
      val ignoreWrite = value(pipeline.data.RS1) === 0
      val illegalScrId = False
      val pcc = value(context.data.PCC)
      val cs1 = value(context.data.CS1_DATA)
      val hasAsr = pcc.perms.accessSystemRegisters
      val needAsr = True
      val cd = Capability.Null

      when (arbitration.isValid && value(Data.SCR_RW)) {
        when(!ignoreRead) {
          switch (scrId) {
            is (ScrIndex.PCC) {
              cd := getPcc(scrStage)
              needAsr := False
            }
            is (ScrIndex.DDC) {
              cd := getDdc(scrStage)
              needAsr := False
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
          // TODO: AccessSystemRegsViolation
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
