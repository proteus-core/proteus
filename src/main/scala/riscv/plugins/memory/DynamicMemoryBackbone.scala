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

      private val movingToCmdBuffer = Flow(UInt(config.dbusConfig.idWidth bits))
      movingToCmdBuffer.setIdle()

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

      // this makes sure that once we assert a command, it stays on the bus until it's acknowledged
      // even if the pipeline is flushed or a command with a lower index comes in
      private val cmdBuffer = Reg(Flow(MemBusCmd(config.dbusConfig)))
      private val currentlySendingIndex = Reg(Flow(UInt(config.dbusConfig.idWidth bits)))

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

          // forwarding the ready signal for the command:
          // 1. if the command is being moved to the buffer
          // 2. if the command was sent immediately, not from the buffer
          when(
            !activeFlush && internalReadDBus.cmd.valid &&
              (
                (movingToCmdBuffer.valid && movingToCmdBuffer.payload === index) ||
                  (!cmdBuffer.valid && currentlyInserting.payload.stageIndex === index && currentlyInserting.valid)
              )
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
              // do not forward if the command was sent from the buffer and it's already invalidated
              busValid := !activeFlush && !(cmdBuffer.valid && busId2StageIndex(nextId).invalidated)
            }
            // TODO: cmdSend could become problematic or it will always waste a cycle (first false for a cycle, then
            // new load can start)
            when(
              busId2StageIndex(unifiedInternalDBus.rsp.id).cmdSent && busId2StageIndex(
                unifiedInternalDBus.rsp.id
              ).stageIndex === index
            ) {
              busId2StageIndex(unifiedInternalDBus.rsp.id).rspReceived := True
              busId2StageIndex(unifiedInternalDBus.rsp.id).cmdSent := False // free up slot
              busValid := !busId2StageIndex(unifiedInternalDBus.rsp.id).invalidated && !activeFlush
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

      // TODO: is it possible to do the following with an arbiter instead of this manual mess?

      private val cmds = fullDBusCmds :+ internalWriteDBus.cmd

      internalWriteDBus.cmd.ready := unifiedInternalDBus.cmd.ready && unifiedInternalDBus.cmd.write

      private var context = when(cmdBuffer.valid) {
        unifiedInternalDBus.cmd.payload := cmdBuffer.payload
        unifiedInternalDBus.cmd.valid := True
        when(unifiedInternalDBus.cmd.ready) {
          currentlySendingIndex.setIdle()
          cmdBuffer.setIdle()
          when(!cmdBuffer.write) {
            nextId.increment()
            busId2StageIndex(nextId).stageIndex := currentlySendingIndex.payload
            when(!sameCycleReturn) {
              busId2StageIndex(nextId).cmdSent := True
            }
            busId2StageIndex(nextId).rspReceived := False
            currentlyInserting.valid := True
            currentlyInserting.payload.stageIndex := currentlySendingIndex.payload
          }
        }
      }

      cmds.zipWithIndex.foreach { case (cmd, index) =>
        context =
          context.elsewhen(cmd.valid && ((!busId2StageIndex(nextId).cmdSent) || cmd.write)) {
            unifiedInternalDBus.cmd.valid := True
            unifiedInternalDBus.cmd.address := cmd.address
            unifiedInternalDBus.cmd.id := nextId
            unifiedInternalDBus.cmd.write := cmd.write
            unifiedInternalDBus.cmd.wdata := cmd.wdata
            unifiedInternalDBus.cmd.wmask := cmd.wmask
            when(!cmd.write) {
              busId2StageIndex(nextId).invalidated := activeFlush
            }
            when(unifiedInternalDBus.cmd.ready) {
              currentlySendingIndex.setIdle()
              cmdBuffer.setIdle()
              when(!cmd.write) {
                nextId.increment()
                busId2StageIndex(nextId).stageIndex := U(index).resized
                when(!sameCycleReturn) {
                  busId2StageIndex(nextId).cmdSent := True
                }
                busId2StageIndex(nextId).rspReceived := False
                currentlyInserting.valid := True
                currentlyInserting.payload.stageIndex := index
              }
            } otherwise {
              movingToCmdBuffer.push(index)
              cmdBuffer.push(unifiedInternalDBus.cmd)
              currentlySendingIndex.push(index)
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
