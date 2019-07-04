package riscv.plugins

import riscv._

import spinal.core._

class DataHazardResolver(implicit config: Config) extends Plugin {
  override def build(pipeline: Pipeline): Unit = {
    pipeline plug new Area {
      import pipeline._

      // Make sure RS* and RD* are passed through the whole pipeline because we
      // need it to resolve RS inputs.
      val lastStage = stages.last
      lastStage.output(data.RS1)
      lastStage.output(data.RS2)
      lastStage.output(data.RD_VALID)
      lastStage.output(data.RD_DATA)
      lastStage.output(data.RD)
    }
  }

  override def finish(pipeline: Pipeline): Unit = {
    pipeline plug new Area {
      import pipeline._

      val trapHandler = pipeline.getService[TrapService]

      // RS forwarding logic:
      //  - Values are forwarded ASAP: all stages are checked if their current
      //    instruction will eventually need RS* (input(RS*) =/= 0). If so, all
      //    later stages are checked to see if they produced a new value for RS*
      //    and if a new value is found, it is used as input(RS*).
      //  - If a stage needs RS* in the current cycle (arbitration.rs*Needed)
      //    and a later stage is found that will produce a new value for RS* but
      //    hasn't done so yet (input(WRITE_RD) && !input(RD_VALID)), the stage
      //    is stalled until the value is produced.
      //  - In this scheme, the if the output of an instruction in WB is needed
      //    by the instruction currently in ID, it cannot be properly forwarded
      //    since the value isn't in the pipeline anymore. Therefore, we add
      //    additional registers (lastWrittenRd) to store the last value written
      //    by WB.
      //  - There is still one issue to be resolved: let's say an instruction's
      //    RS1 is forwarded from lastWrittenRd and the instruction needs to
      //    stall a cycle for its RS2 to be available. After the stall cycle,
      //    the RS1 value will be moved out of the pipeline and cannot be
      //    forwarded anymore. To solve this, this instruction's RS1 value will
      //    be moved in the RS1_DATA pipeline register before its stage so that
      //    it will still be available in the next cycle.
      val lastWrittenRd = new Bundle {
        val id = Reg(pipeline.data.RD.dataType())
        val valid = Reg(pipeline.data.RD_VALID.dataType())
        val data = Reg(pipeline.data.RD_DATA.dataType())
      }

      when (stages.last.arbitration.isDone &&
            !trapHandler.hasTrapped(pipeline, stages.last)) {
        lastWrittenRd.id := stages.last.output(pipeline.data.RD)
        lastWrittenRd.valid := stages.last.output(pipeline.data.RD_VALID)
        lastWrittenRd.data := stages.last.output(pipeline.data.RD_DATA)
      }

      for (stage <- stages.dropWhile(_ != execute)) {
        case class ResolveInfo(forwarded: Bool, value: UInt)

        def resolveRs(rsNeeded: Bool, rs: PipelineData[UInt],
                      rsData: PipelineData[UInt]): ResolveInfo = {
          if (stage.hasInput(rsData)) {
            val info = ResolveInfo(False, 0)
            val neededRs = stage.input(rs)

            when (stage.arbitration.isValid) {
              when (neededRs =/= 0) {
                when (lastWrittenRd.valid && lastWrittenRd.id === neededRs) {
                  info.forwarded := True
                  info.value := lastWrittenRd.data
                }

                val nextStages = pipeline.stages.dropWhile(_ != stage).tail.reverse

                for (nextStage <- nextStages) {
                  when (nextStage.arbitration.isValid &&
                        !trapHandler.hasTrapped(pipeline, nextStage) &&
                        nextStage.input(pipeline.data.WRITE_RD) &&
                        nextStage.input(pipeline.data.RD) === neededRs) {
                    when (nextStage.input(pipeline.data.RD_VALID)) {
                      info.forwarded := True
                      info.value := nextStage.input(pipeline.data.RD_DATA)
                    } elsewhen (rsNeeded) {
                      info.forwarded := False
                      stage.arbitration.isStalled := True
                    }
                  }
                }

                when (info.forwarded) {
                  stage.input(rsData) := info.value
                }
              }
            }

            info
          } else {
            null
          }
        }

        val rs1Info = resolveRs(stage.arbitration.rs1Needed, data.RS1, data.RS1_DATA)
        val rs2Info = resolveRs(stage.arbitration.rs2Needed, data.RS2, data.RS2_DATA)

        if (rs1Info != null || rs2Info != null) {
          when(stage.arbitration.isStalled || !stage.arbitration.isReady) {
            val prevStage = stages.takeWhile(_ != stage).last
            val prevStageRegs = pipelineRegs(prevStage)

            if (rs1Info != null) {
              when (rs1Info.forwarded) {
                prevStageRegs.setReg(data.RS1_DATA, rs1Info.value)
              }
            }
            if (rs2Info != null) {
              when (rs2Info.forwarded) {
                prevStageRegs.setReg(data.RS2_DATA, rs2Info.value)
              }
            }
          }
        }
      }
    }
  }
}
