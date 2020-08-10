package riscv.plugins.capabilities

import riscv._
import spinal.core._

class ScrFile(scrStage: Stage)(implicit context: Context) extends Plugin[Pipeline] {
  object Data {
    object SCR_RW extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    val decoder = pipeline.getService[DecoderService]

    decoder.configure {config =>
      config.addDefault(Map(
        Data.SCR_RW -> False
      ))

      config.addDecoding(Opcodes.CSpecialRW, InstructionType.R, Map(
        Data.SCR_RW -> True,
        context.data.WRITE_CD -> True
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

      val scrId = value(context.data.CS2)
      val ignoreRead = value(context.data.CD) === 0
      val ignoreWrite = value(context.data.CS1) === 0
      val illegalScrId = False
      val pcc = value(context.data.PCC)
      val hasAsr = pcc.perms.accessSystemRegisters
      val needAsr = True
      val cd = Capability.Null

      when (arbitration.isValid && value(Data.SCR_RW)) {
        when(!ignoreRead) {
          switch (scrId) {
            is (ScrIndex.PCC) {
              cd.assignFrom(pcc, value(pipeline.data.PC))
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
            default {
              illegalScrId := True
            }
          }
        }

        when (illegalScrId) {
          val trapHandler = pipeline.getService[TrapService]
          val ir = value(pipeline.data.IR)
          trapHandler.trap(scrStage, TrapCause.IllegalInstruction(ir))
        } elsewhen (needAsr && !hasAsr) {
          // TODO: AccessSystemRegsViolation
        } elsewhen (!ignoreRead) {
          output(context.data.CD_DATA) := cd
        }
      }
    }
  }
}
