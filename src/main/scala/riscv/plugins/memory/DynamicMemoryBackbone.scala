package riscv.plugins.memory

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

class DynamicMemoryBackbone(stageCount: Int)(implicit config: Config)
    extends MemoryBackbone
    with Resettable {

  private var activeFlush: Bool = null

  override def build(): Unit = {
    pipeline plug new Area {
      val activeFlushCopy = Bool()
      activeFlush = activeFlushCopy
      activeFlush := False
    }
  }

  override def finish(): Unit = {
    super.finish()

    pipeline plug new Area {
      externalDBus = master(new MemBus(config.dbusConfig)).setName("dbus")

      private val unifiedInternalDBus = Stream(MemBus(config.dbusConfig))

      unifiedInternalDBus.cmd.valid := False
      unifiedInternalDBus.cmd.address.assignDontCare()
      unifiedInternalDBus.cmd.id.assignDontCare()
      unifiedInternalDBus.cmd.write := False
      unifiedInternalDBus.cmd.wdata.assignDontCare()
      unifiedInternalDBus.cmd.wmask.assignDontCare()

      unifiedInternalDBus.rsp.ready := False

      private case class IdMeta() extends Bundle {
        val stageIndex = UInt(config.dbusConfig.idWidth bits)
        val cmdSent = Bool()
        val rspReceived = Bool()
        val invalidated = Bool()
      }

      private val currentlyInserting = Flow(IdMeta())
      currentlyInserting.valid := False
      currentlyInserting.payload.assignDontCare()

      private val sameCycleReturn = Bool()
      sameCycleReturn := False

      private val nextId = Counter(config.dbusConfig.idWidth bits)
      private val busId2StageIndex = Vec.fill(
        UInt(config.dbusConfig.idWidth bits).maxValue.intValue() + 1
      )(RegInit(IdMeta().getZero))

      for (i <- 0 until stageCount) {
        when(activeFlush) {
          busId2StageIndex(i).invalidated := True
        }
      }
      // when putting a cmd on external, add stage index with counter.next
      // when pipeline is invalidated, invalidate all entries in here
      // when getting a response, if it's invalid, throw it away
      // when counter is full (all entries are waiting), don't accept any cmd

      private val fullDBusCmds = internalReadDBuses.zipWithIndex.map {
        case (internalReadDBus, index) =>
          val fullReadDBusCmd = Stream(MemBusCmd(config.dbusConfig))
          fullReadDBusCmd.valid := internalReadDBus.cmd.valid
          fullReadDBusCmd.id := internalReadDBus.cmd.id
          internalReadDBus.cmd.ready := False
          fullReadDBusCmd.write := False
          fullReadDBusCmd.wmask.assignDontCare()
          fullReadDBusCmd.wdata.assignDontCare()
          fullReadDBusCmd.address := internalReadDBus.cmd.address

          val busValid = Bool()
          busValid := False

          when(
            currentlyInserting.payload.stageIndex === index && currentlyInserting.valid && !activeFlush
          ) {
            internalReadDBus.cmd.ready := True
          }

          // only set valid bit for the corresponding load bus
          when(unifiedInternalDBus.rsp.valid) {
            when(
              currentlyInserting.valid && currentlyInserting.stageIndex === index && nextId === unifiedInternalDBus.rsp.id
            ) {
              sameCycleReturn := True
              busId2StageIndex(unifiedInternalDBus.rsp.id).rspReceived := True
              busId2StageIndex(unifiedInternalDBus.rsp.id).cmdSent := False // free up slot
              busValid := True
            }
            when(
              busId2StageIndex(unifiedInternalDBus.rsp.id).cmdSent && busId2StageIndex(
                unifiedInternalDBus.rsp.id
              ).stageIndex === index
            ) {
              busId2StageIndex(unifiedInternalDBus.rsp.id).rspReceived := True
              busId2StageIndex(unifiedInternalDBus.rsp.id).cmdSent := False // free up slot
              when(!busId2StageIndex(unifiedInternalDBus.rsp.id).invalidated) {
                busValid := True
              }
            }
          }

          internalReadDBus.rsp.valid := busValid
          internalReadDBus.rsp.payload := unifiedInternalDBus.rsp.payload

          fullReadDBusCmd
      }

      // check whether the correct load bus is ready to receive
      when(unifiedInternalDBus.rsp.valid) {
        unifiedInternalDBus.rsp.ready := internalReadDBuses(
          busId2StageIndex(unifiedInternalDBus.rsp.id).stageIndex
        ).rsp.ready
      }

      private var context = when(False) {}

      // TODO: is it possible to do the following with an arbiter instead of this manual mess?

      private val cmds = fullDBusCmds :+ internalWriteDBus.cmd

      internalWriteDBus.cmd.ready := unifiedInternalDBus.cmd.ready && unifiedInternalDBus.cmd.write

      cmds.zipWithIndex.foreach { case (cmd, index) =>
        context =
          context.elsewhen(cmd.valid && ((!busId2StageIndex(nextId).cmdSent) || cmd.write)) {
            unifiedInternalDBus.cmd.valid := True
            unifiedInternalDBus.cmd.address := cmd.address
            unifiedInternalDBus.cmd.id := nextId
            unifiedInternalDBus.cmd.write := cmd.write
            unifiedInternalDBus.cmd.wdata := cmd.wdata
            unifiedInternalDBus.cmd.wmask := cmd.wmask
            when(unifiedInternalDBus.cmd.ready) {
              when(!cmd.write) {
                nextId.increment()
                busId2StageIndex(nextId).stageIndex := U(index).resized
                when(!sameCycleReturn) {
                  busId2StageIndex(nextId).cmdSent := True
                }
                busId2StageIndex(nextId).rspReceived := False
                busId2StageIndex(nextId).invalidated := activeFlush
                currentlyInserting.valid := True
                currentlyInserting.payload.stageIndex := index
              }
            }
          }
      }

      dbusFilter.foreach(_(internalWriteDBusStage, unifiedInternalDBus, externalDBus))
      dbusObservers.foreach(_(internalWriteDBusStage, unifiedInternalDBus))
    }
  }

  override def createInternalDBus(
      readStages: Seq[Stage],
      writeStage: Stage
  ): (Seq[MemBus], MemBus) = {
    assert(!readStages.contains(writeStage))

    internalReadDBuses = readStages.map(readStage => {
      val readArea = readStage plug new Area {
        val dbus = master(new MemBus(config.readDbusConfig))
      }
      readArea.dbus
    })

    writeStage plug new Area {
      internalWriteDBus = master(new MemBus(config.dbusConfig))
    }

    pipeline plug {
      internalWriteDBus.rsp.valid := False
    }

    internalReadDBusStages = readStages
    internalWriteDBusStage = writeStage

    (internalReadDBuses, internalWriteDBus)
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
