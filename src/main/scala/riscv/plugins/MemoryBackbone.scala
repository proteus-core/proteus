package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

import scala.collection.mutable

class MemoryBackbone(implicit config: Config) extends Plugin with MemoryService {
  private var externalIBus: MemBus = null
  private var externalDBus: MemBus = null
  private var internalIBus: MemBus = null
  private var internalDBus: MemBus = null
  private var internalDBusStage: Stage = null
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

      if (internalDBus != null) {
        dbusFilter.get (internalDBusStage, internalDBus, externalDBus)
        dbusObservers.foreach(_ (internalDBusStage, internalDBus))
      } else {
        internalDBus = externalDBus
        dummyConnect(internalDBus)
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
    }

    internalIBus
  }

  override def createInternalDBus(stage: Stage): MemBus = {
    assert(internalDBus == null)

    stage plug new Area {
      internalDBus = master(new MemBus(config.dbusConfig))
    }

    internalDBusStage = stage
    internalDBus
  }

  override def getDBusStages: Seq[Stage] = {
    if (internalDBus == null) {
      Seq()
    } else {
      Seq(internalDBusStage)
    }
  }

  override def filterDBus(filter: MemBusFilter): Unit = {
    assert(dbusFilter.isEmpty)
    dbusFilter = Some(filter)
  }

  override def observeDBus(observer: MemBusObserver): Unit = {
    dbusObservers += observer
  }
}
