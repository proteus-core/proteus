package riscv.plugins

import riscv._

import spinal.core._

class DataHazardResolver extends Plugin {
  override def build(pipeline: Pipeline, config: Config): Unit = {
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

  override def finish(pipeline: Pipeline, config: Config): Unit = {
    pipeline plug new Area {
      import pipeline._

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
      val lastWrittenRd = new Bundle {
        val id = Reg(pipeline.data.RD.dataType())
        val valid = Reg(pipeline.data.RD_VALID.dataType())
        val data = Reg(pipeline.data.RD_DATA.dataType())
      }

      when (stages.last.arbitration.isDone) {
        lastWrittenRd.id := stages.last.output(pipeline.data.RD)
        lastWrittenRd.valid := stages.last.output(pipeline.data.RD_VALID)
        lastWrittenRd.data := stages.last.output(pipeline.data.RD_DATA)
      }

      for (stage <- stages.dropWhile(_ != execute)) {
        def resolveRs(rsNeeded: Bool, rs: PipelineData[UInt],
                      rsData: PipelineData[UInt]): Unit = {
          if (stage.hasInput(rsData)) {
            val neededRs = stage.input(rs)
            val neededRsData = stage.input(rsData)

            when (neededRs =/= 0) {
              when (lastWrittenRd.valid && lastWrittenRd.id === neededRs) {
                neededRsData := lastWrittenRd.data
              }

              val nextStages = pipeline.stages.dropWhile(_ != stage).tail.reverse

              for (nextStage <- nextStages) {
                when (nextStage.arbitration.isValid &&
                      nextStage.input(pipeline.data.WRITE_RD) &&
                      nextStage.input(pipeline.data.RD) === neededRs) {
                  when (nextStage.input(pipeline.data.RD_VALID)) {
                    neededRsData := nextStage.input(pipeline.data.RD_DATA)
                  } elsewhen (rsNeeded) {
                    stage.arbitration.isStalled := True
                  }
                }
              }
            }
          }
        }

        resolveRs(stage.arbitration.rs1Needed, data.RS1, data.RS1_DATA)
        resolveRs(stage.arbitration.rs2Needed, data.RS2, data.RS2_DATA)
      }
    }
  }
}
