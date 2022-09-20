package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

class DynamicMemoryBackbone(stageCount: Int)(implicit config: Config)
    extends Plugin
    with MemoryService {
  private val idWidth: BitCount = log2Up(stageCount) bits

  private var externalIBus: MemBus = null
  private var internalIBus: MemBus = null
  private var externalDBus: MemBus = null
  private var internalReadDBuses: Seq[MemBus] = null
  private var internalWriteDBus: MemBus = null
  private var internalReadDBusStages: Seq[Stage] = null
  private var internalWriteDBusStage: Stage = null
  private var dbusFilter: Option[MemBusFilter] = None
  private val dbusObservers = mutable.ArrayBuffer[MemBusObserver]()

  override def finish(): Unit = {
    def dummyConnect(bus: MemBus) = {
      bus.cmd.valid := False
      bus.cmd.payload.assignDontCare()
      bus.rsp.ready := False
    }

    // IBUS
    pipeline plug new Area {
      externalIBus = master(new MemBus(config.ibusConfig, idWidth)).setName("ibus")

      if (internalIBus != null) {
        externalIBus <> internalIBus
      } else {
        internalIBus = externalIBus
        dummyConnect(internalIBus)
      }
    }

    // DBUS
    if (dbusFilter.isEmpty) {
      dbusFilter = Some((_, idbus, edbus) => {
        idbus <> edbus
      })
    }

    pipeline plug new Area {
      externalDBus = master(new MemBus(config.dbusConfig, idWidth)).setName("dbus")

      val pendingCount = Vec.fill(stageCount)(RegInit(UInt(3 bits).getZero)) // 3 bits: guess
      val increaseCount = Vec.fill(stageCount)(False)
      val decreaseCount = Vec.fill(stageCount)(False)

      // Create a RW version of the RO internalReadDBus so that we can use it with
      // StreamArbiterFactory
      val fullDBusCmds = internalReadDBuses.zipWithIndex.map { case (internalReadDBus, index) =>
        val fullReadDBusCmd = Stream(MemBusCmd(config.dbusConfig, idWidth))
        fullReadDBusCmd.valid := internalReadDBus.cmd.valid
        fullReadDBusCmd.id := internalReadDBus.cmd.id
        internalReadDBus.cmd.ready := fullReadDBusCmd.ready
        fullReadDBusCmd.write := False
        fullReadDBusCmd.wmask.assignDontCare()
        fullReadDBusCmd.wdata.assignDontCare()
        fullReadDBusCmd.address := internalReadDBus.cmd.address

        val busValid = Bool()
        busValid := False

        // only set valid bit for the corresponding load bus
        when(externalDBus.rsp.id === index && externalDBus.rsp.valid) {
          when(pendingCount(index) === 1 && !internalReadDBus.cmd.valid) { // only forward the response if there's no contending load and it also wasn't issued in this same cycle
            busValid := externalDBus.rsp.valid
          }
          decreaseCount(index) := True
        }

        busValid <> internalReadDBus.rsp.valid
        externalDBus.rsp.payload <> internalReadDBus.rsp.payload

        fullReadDBusCmd

      // TODO filter and observers
      }

      val rspReady = Bool()
      rspReady := False

      // check whether the correct load bus is ready to receive
      when(externalDBus.rsp.valid) {
        rspReady := internalReadDBuses(externalDBus.rsp.id.resized).rsp.ready
      }

      externalDBus.rsp.ready <> rspReady

      // TODO: is it possible to do the following with an arbiter instead of this manual mess?

      val cmds = fullDBusCmds :+ internalWriteDBus.cmd

      val cmdValid = Bool()
      cmdValid := False
      val cmdAddress = UInt(config.xlen bits)
      cmdAddress.assignDontCare()
      val cmdId = UInt(idWidth)
      cmdId.assignDontCare()
      val cmdWrite = Bool()
      cmdWrite := False
      val cmdWdata = UInt(config.dbusConfig.dataWidth bits)
      cmdWdata.assignDontCare()
      val cmdWmask = Bits(config.dbusConfig.dataWidth / 8 bits)
      cmdWmask.assignDontCare()

      var context = when(False) {}

      cmds.zipWithIndex.foreach { case (cmd, index) =>
        val ready = Bool()
        ready := False

        when(externalDBus.cmd.id === index) {
          ready := externalDBus.cmd.ready
        }

        cmd.ready := ready

        context = context.elsewhen(
          cmd.valid && ((pendingCount(index) < pendingCount(index).maxValue) || cmd.write)
        ) { // prevent overflowing the pending counter for loads
          cmdValid := True
          cmdAddress := cmd.address
          cmdId := index
          cmdWrite := cmd.write
          cmdWdata := cmd.wdata
          cmdWmask := cmd.wmask
          when(ready) {
            increaseCount(index) := True
          }
        }
      }

      externalDBus.cmd.valid <> cmdValid
      externalDBus.cmd.address <> cmdAddress
      externalDBus.cmd.id <> cmdId
      externalDBus.cmd.write <> cmdWrite
      externalDBus.cmd.wdata <> cmdWdata
      externalDBus.cmd.wmask <> cmdWmask

      for (i <- 0 until stageCount) {
        when(increaseCount(i) && !decreaseCount(i)) {
          pendingCount(i) := pendingCount(i) + 1
        }
        when(!increaseCount(i) && decreaseCount(i)) {
          pendingCount(i) := pendingCount(i) - 1
        }
      }
    }
  }

  override def getExternalIBus: MemBus = {
    assert(externalIBus != null)
    externalIBus
  }

  override def getExternalDBus: MemBus = {
    assert(externalDBus != null)
    externalDBus
  }

  override def createInternalIBus(stage: Stage): MemBus = {
    assert(internalIBus == null)

    stage plug new Area {
      internalIBus = master(new MemBus(config.ibusConfig, idWidth))
      internalIBus.cmd.id.assignDontCare()
    }

    internalIBus
  }

  override def createInternalDBus(
      readStages: Seq[Stage],
      writeStage: Stage
  ): (Seq[MemBus], MemBus) = {
    assert(!readStages.contains(writeStage))

    internalReadDBuses = readStages.map(readStage => {
      val readArea = readStage plug new Area {
        val dbus = master(new MemBus(config.readDbusConfig, idWidth))
      }
      readArea.dbus
    })

    writeStage plug new Area {
      internalWriteDBus = master(new MemBus(config.dbusConfig, idWidth))
    }

    pipeline plug {
      internalWriteDBus.rsp.valid := False
    }

    internalReadDBusStages = readStages
    internalWriteDBusStage = writeStage

    (internalReadDBuses, internalWriteDBus)
  }

  override def getDBusStages: Seq[Stage] = {
    internalReadDBusStages.filter(_ != null).distinct // TODO: not sure what this does
  }

  override def filterDBus(filter: MemBusFilter): Unit = {
    assert(dbusFilter.isEmpty)
    dbusFilter = Some(filter)
  }

  override def observeDBus(observer: MemBusObserver): Unit = {
    dbusObservers += observer
  }
}
