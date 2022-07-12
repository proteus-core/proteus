package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

import scala.collection.mutable

class MemoryBackbone(implicit config: Config) extends Plugin with MemoryService {
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
      externalIBus = master(new MemBus(config.ibusConfig)).setName("ibus")

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
      externalDBus = master(new MemBus(config.dbusConfig)).setName("dbus")

      if (internalReadDBuses.contains(internalWriteDBus)) {  // TODO: update this to also support multiple loads?
        dbusFilter.foreach(_(internalWriteDBusStage, internalWriteDBus, externalDBus))
        dbusObservers.foreach(_(internalWriteDBusStage, internalWriteDBus))
      } else {
        // Create a RW version of the RO internalReadDBus so that we can use it with
        // StreamArbiterFactory
        val fullDBusCmds = internalReadDBuses.zipWithIndex.map(tuple => {
          val internalReadDBus = tuple._1
          val index = tuple._2

          val fullReadDBusCmd = Stream(MemBusCmd(config.dbusConfig))
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
          when (externalDBus.rsp.id === index) {
            busValid := externalDBus.rsp.valid
          }

          busValid <> internalReadDBus.rsp.valid
          externalDBus.rsp.payload <> internalReadDBus.rsp.payload

          fullReadDBusCmd

          // TODO filter and observers
        }
        )

        val rspReady = Bool()
        rspReady := False

        // check whether the correct load bus is ready to receive
        when (externalDBus.rsp.valid) {
          rspReady := internalReadDBuses(externalDBus.rsp.id.resized).rsp.ready
        }

        externalDBus.rsp.ready <> rspReady

        // TODO: is it possible to do the following with an arbiter instead of this manual mess?

        val cmds = fullDBusCmds :+ internalWriteDBus.cmd

        val cmdValid = Bool()
        cmdValid := False
        val cmdAddress = UInt(config.xlen bits)
        cmdAddress.assignDontCare()
        val cmdId = UInt(Constants.ID_WIDTH bits)
        cmdId.assignDontCare()
        val cmdWrite = Bool()
        cmdWrite := False
        val cmdWdata = UInt(config.dbusConfig.dataWidth bits)
        cmdWdata.assignDontCare()
        val cmdWmask = Bits(config.dbusConfig.dataWidth / 8 bits)
        cmdWmask.assignDontCare()

        var context = when (False) {}

        cmds.zipWithIndex.foreach(tuple => {
          val cmd = tuple._1
          val index = tuple._2

          val ready = Bool()
          ready := False

          when (externalDBus.cmd.id === index) {
            ready := externalDBus.cmd.ready
          }

          cmd.ready := ready

          context = context.elsewhen(cmd.valid) {
            cmdValid := True
            cmdAddress := cmd.address
            cmdId := index
            cmdWrite := cmd.write
            cmdWdata := cmd.wdata
            cmdWmask := cmd.wmask
          }
        })

        externalDBus.cmd.valid <> cmdValid
        externalDBus.cmd.address <> cmdAddress
        externalDBus.cmd.id <> cmdId
        externalDBus.cmd.write <> cmdWrite
        externalDBus.cmd.wdata <> cmdWdata
        externalDBus.cmd.wmask <> cmdWmask
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
      internalIBus = master(new MemBus(config.ibusConfig))
      internalIBus.cmd.id.assignDontCare()
    }

    internalIBus
  }

  override def createInternalDBus(readStages: Seq[Stage], writeStage: Stage): (Seq[MemBus], MemBus) = {
    internalReadDBuses = readStages.map(readStage => {
      val readArea = readStage plug new Area {
        val dbusConfig = if (readStage == writeStage) {
          config.dbusConfig
        } else {
          config.readDbusConfig
        }
        val dbus = master(new MemBus(dbusConfig))
      }
      readArea.dbus
      }
    )

    val writeIndex = readStages.indexOf(writeStage)

    internalWriteDBus = if (writeIndex != -1) {
      internalReadDBuses(writeIndex)
    } else {
      val writeArea = writeStage plug new Area {
        val dbus = master(new MemBus(config.dbusConfig))
      }

      pipeline plug {
        writeArea.dbus.rsp.valid := False
      }

      writeArea.dbus
    }

    internalReadDBusStages = readStages
    internalWriteDBusStage = writeStage

    (internalReadDBuses, internalWriteDBus)
  }

  override def getDBusStages: Seq[Stage] = {
    (internalReadDBusStages ++ internalReadDBusStages).filter(_ != null).distinct  // TODO: not sure what this does
  }

  override def filterDBus(filter: MemBusFilter): Unit = {
    assert(dbusFilter.isEmpty)
    dbusFilter = Some(filter)
  }

  override def observeDBus(observer: MemBusObserver): Unit = {
    dbusObservers += observer
  }
}
