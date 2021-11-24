package riscv.plugins.scheduling.static

import riscv._

import spinal.core._

import scala.collection.mutable

// FIXME firstRsReadStage could be automatically deduced by registering which
// plugins write to arbitration.rsXNeeded.
class DataHazardResolver(firstRsReadStage: Stage) extends Plugin[StaticPipeline] with DataHazardService {
  private val hazardInfos = mutable.ArrayBuffer[DataHazardInfo[Data]]()

  override def addHazard[T <: Data](info: DataHazardInfo[T]): Unit = {
    hazardInfos += info.asInstanceOf[DataHazardInfo[Data]]
  }

  override def build(): Unit = {
    pipeline plug new Area {
      // Make sure RS* and RD* are passed through the whole pipeline because we
      // need it to resolve RS inputs.
      val lastStage = pipeline.stages.last
      lastStage.output(pipeline.data.RS1)
      lastStage.output(pipeline.data.RS1_TYPE)
      lastStage.output(pipeline.data.RS2)
      lastStage.output(pipeline.data.RS2_TYPE)
      lastStage.output(pipeline.data.RD_DATA_VALID)
      lastStage.output(pipeline.data.RD)
      lastStage.output(pipeline.data.RD_TYPE)
    }
  }

  override def finish(): Unit = {
    for (info <- hazardInfos) {
      resolveHazard(info)
    }
  }

  def resolveHazard[T <: Data](hazardInfo: DataHazardInfo[T]) {
    pipeline plug new Area {
      val trapHandler = pipeline.getService[TrapService]

      // RS forwarding logic:
      //  - Values are forwarded ASAP: all stages are checked if their current
      //    instruction will eventually need RS* (input(RS*) =/= 0). If so, all
      //    later stages are checked to see if they produced a new value for RS*
      //    and if a new value is found, it is used as input(RS*).
      //  - If a stage needs RS* in the current cycle (arbitration.rs*Needed)
      //    and a later stage is found that will produce a new value for RS* but
      //    hasn't done so yet (input(WRITE_RD) && !input(RD_DATA_VALID)), the
      //    stage is stalled until the value is produced.
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
        val valid = Reg(pipeline.data.RD_DATA_VALID.dataType())
        val data = Reg(hazardInfo.rdData.dataType())
      }

      val stages = pipeline.stages

      when (stages.last.arbitration.isDone &&
            !trapHandler.hasTrapped(stages.last)) {
        lastWrittenRd.id := stages.last.output(pipeline.data.RD)
        lastWrittenRd.valid :=
          stages.last.output(pipeline.data.RD_DATA_VALID) &&
          stages.last.output(pipeline.data.RD_TYPE) === hazardInfo.registerType
        lastWrittenRd.data := stages.last.output(hazardInfo.rdData)
      }

      for (stage <- stages.dropWhile(_ != firstRsReadStage)) {
        case class ResolveInfo(forwarded: Bool, value: T)

        def resolveRs(rsNeeded: Bool, rs: PipelineData[UInt],
                      rsType: PipelineData[SpinalEnumCraft[RegisterType.type]],
                      rsData: PipelineData[T]): ResolveInfo = {
          if (stage.hasInput(rsData)) {
            val info = ResolveInfo(False, hazardInfo.rdData.dataType().assignDontCare())
            val neededRs = stage.input(rs)

            when (stage.arbitration.isValid) {
              when (neededRs =/= 0 && stage.input(rsType) === hazardInfo.registerType) {
                when (lastWrittenRd.valid && lastWrittenRd.id === neededRs) {
                  info.forwarded := True
                  info.value := lastWrittenRd.data
                }

                val nextStages = pipeline.stages.dropWhile(_ != stage).tail.reverse

                for (nextStage <- nextStages) {
                  when (nextStage.arbitration.isValid &&
                        !trapHandler.hasTrapped(nextStage) &&
                        nextStage.input(pipeline.data.RD_TYPE) === hazardInfo.registerType &&
                        nextStage.input(pipeline.data.RD) === neededRs) {
                    when (nextStage.input(pipeline.data.RD_DATA_VALID)) {
                      info.forwarded := True
                      info.value := nextStage.input(hazardInfo.rdData)
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

        val rs1Info = resolveRs(stage.arbitration.rs1Needed, pipeline.data.RS1, pipeline.data.RS1_TYPE, hazardInfo.rs1Data)
        val rs2Info = resolveRs(stage.arbitration.rs2Needed, pipeline.data.RS2, pipeline.data.RS2_TYPE, hazardInfo.rs2Data)

        if (rs1Info != null || rs2Info != null) {
          when(stage.arbitration.isStalled || !stage.arbitration.isReady) {
            val prevStage = stages.takeWhile(_ != stage).last
            val prevStageRegs = pipeline.pipelineRegs(prevStage)

            if (rs1Info != null) {
              when (rs1Info.forwarded) {
                prevStageRegs.setReg(hazardInfo.rs1Data, rs1Info.value)
              }
            }
            if (rs2Info != null) {
              when (rs2Info.forwarded) {
                prevStageRegs.setReg(hazardInfo.rs2Data, rs2Info.value)
              }
            }
          }
        }
      }
    }
  }

  override def resolveHazard(conflicts: (Stage, Seq[Stage]) => Bool): Unit = {
    pipeline plug new Area {
      val stages = pipeline.stages

      for (stage <- stages) {
        val nextStages = stages.dropWhile(_ != stage).tail

        when (conflicts(stage, nextStages)) {
          stage.arbitration.isStalled := True
        }
      }
    }
  }
}
