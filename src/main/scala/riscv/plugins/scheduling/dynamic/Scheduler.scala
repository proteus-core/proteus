package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._

class Scheduler() extends Plugin[DynamicPipeline] with IssueService {

  private object PrivateRegisters {
    object DEST_FU extends PipelineData(Bits(pipeline.rsStages.size bits))
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(PrivateRegisters.DEST_FU, B(0))
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val cdbBMetaData = new DynBundle[PipelineData[spinal.core.Data]]
      val registerBundle = new DynBundle[PipelineData[spinal.core.Data]]

      private val ret = pipeline.retirementStage
      private val ls = pipeline.loadStages.head // TODO !!!
      for (
        register <-
          ret.lastValues.keys.toSet union ret.outputs.keys.toSet union ls.lastValues.keys.toSet union ls.outputs.keys.toSet
      ) {
        registerBundle.addElement(register, register.dataType)
      }

      pipeline.rob = new ReorderBuffer(pipeline, 16, registerBundle, cdbBMetaData)

      private val rob = pipeline.rob
      rob.build()

      private val reservationStations = pipeline.rsStages.map(stage =>
        new ReservationStation(stage, rob, pipeline, registerBundle, cdbBMetaData)
      )

      private val loadManagers = pipeline.loadStages.map(stage =>
        new LoadManager(pipeline, stage, rob, registerBundle, cdbBMetaData)
      )

      val cdb = new CommonDataBus(reservationStations, rob, cdbBMetaData, loadManagers.size)
      cdb.build()
      for ((rs, index) <- reservationStations.zipWithIndex) {
        rs.build()
        rs.cdbStream >> cdb.inputs(index)
      }

      val robDataBus = new RobDataBus(rob, registerBundle, loadManagers.size)
      robDataBus.build()

      for (index <- loadManagers.indices) {
        loadManagers(index).build()
        loadManagers(index).cdbStream >> cdb.inputs(reservationStations.size + index)
        loadManagers(index).rdbStream >> robDataBus.inputs(1 + index)
      }

      val dispatcher = new Dispatcher(pipeline, rob, loadManagers, registerBundle)
      dispatcher.build()

      val dispatchBus = new DispatchBus(reservationStations, rob, dispatcher, registerBundle)
      dispatchBus.build()

      dispatcher.rdbStream >> robDataBus.inputs(0)

      for ((rs, index) <- reservationStations.zipWithIndex) {
        rs.dispatchStream >> dispatchBus.inputs(index)
      }

      pipeline.components = reservationStations ++ loadManagers :+ dispatcher

      // Dispatch
      private val issueStage = pipeline.issuePipeline.stages.last
      issueStage.arbitration.isStalled := False

      when(issueStage.arbitration.isValid && issueStage.arbitration.isReady) {
        val fuMask = issueStage.output(PrivateRegisters.DEST_FU)
        val illegalInstruction = fuMask === 0

        var context = when(False) {}

        for ((rs, index) <- reservationStations.zipWithIndex) {
          context = context.elsewhen(
            (fuMask(index) || illegalInstruction) && rs.isAvailable && rob.isAvailable
          ) {
            rs.execute()
          }
        }

        context.otherwise {
          issueStage.arbitration.isStalled := True
        }
      }
    }
  }

  override def setDestinations(opcode: MaskedLiteral, stages: Set[Stage]): Unit = {
    for (stage <- stages) {
      assert(pipeline.rsStages.contains(stage), s"Stage ${stage.stageName} is not an execute stage")
    }

    pipeline.service[DecoderService].configure { config =>
      var fuMask = 0

      for (exeStage <- pipeline.rsStages.reverse) {
        val nextBit = if (stages.contains(exeStage)) 1 else 0
        fuMask = (fuMask << 1) | nextBit
      }

      config.addDecoding(opcode, Map(PrivateRegisters.DEST_FU -> B(fuMask)))
    }
  }
}
