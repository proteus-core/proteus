package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._

class Scheduler(implicit val robCapacity: Int = 8) extends Plugin[DynamicPipeline] with IssueService {
  class RegisterFile extends Area { // TODO: not needed?
    val registerArray = Vec.fill(config.numRegs)(Reg(UInt(config.xlen bits)).init(0))

    def getValue(regId: UInt): UInt = {
      val ret = UInt(config.xlen bits)
      ret := 0
      when (regId =/= 0) {
        ret := registerArray(regId)
      }
      ret
    }

    def updateValue(regId: UInt, value: UInt): Unit = {
      registerArray(regId) := value
    }
  }

  class Data { // TODO: better name for this?
    object DEST_FU extends PipelineData(Bits(pipeline.exeStages.size bits))
  }

  lazy val data = new Data // TODO: better name for this?

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(data.DEST_FU, B(0))
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val registerFile = new RegisterFile
      val rob = new ReorderBuffer(pipeline, registerFile, robCapacity)
      val reservationStations = pipeline.exeStages.map(stage => new ReservationStation(stage, registerFile, rob, pipeline))
      val cdb = new CommonDataBus(reservationStations, rob)
      rob.build()
      cdb.build()
      for ((rs, index) <- reservationStations.zipWithIndex) {
        rs.build()
        rs.cdbStream >> cdb.inputs(index)
      }

      // Dispatch
      val dispatchStage = pipeline.issuePipeline.stages.last
      dispatchStage.arbitration.isStalled := False

      when (dispatchStage.arbitration.isValid && dispatchStage.arbitration.isReady) {
        val fuMask = dispatchStage.output(data.DEST_FU)

        var context = when (fuMask === 0) {
          // TODO Illegal instruction
          dispatchStage.arbitration.isStalled := True
        }

        for ((rs, index) <- reservationStations.zipWithIndex) {
          context = context.elsewhen (fuMask(index) && rs.isAvailable && rob.isAvailable) {
            rs.execute()
          }
        }

        context.otherwise {
          dispatchStage.arbitration.isStalled := True
        }
      }
    }
  }

  override def setDestinations(opcode: MaskedLiteral, stages: Set[Stage]): Unit = {
    for (stage <- stages) {
      assert(pipeline.exeStages.contains(stage),
        s"Stage ${stage.stageName} is not an execute stage")
    }

    pipeline.getService[DecoderService].configure { config =>
      var fuMask = 0

      for (exeStage <- pipeline.exeStages.reverse) {
        val nextBit = if (stages.contains(exeStage)) 1 else 0
        fuMask = (fuMask << 1) | nextBit
      }

      config.addDecoding(opcode, Map(data.DEST_FU -> B(fuMask)))
    }
  }
}
