package riscv.plugins.memory

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

abstract class MemoryBackbone(implicit config: Config) extends Plugin with MemoryService {

  var externalIBus: MemBus = null
  var internalIBus: MemBus = null
  var externalDBus: MemBus = null
  var internalReadDBuses: Seq[MemBus] = null
  var internalWriteDBus: MemBus = null
  var internalReadDBusStages: Seq[Stage] = null
  var internalWriteDBusStage: Stage = null
  var dbusFilter: Option[MemBusFilter] = None
  var ibusFilter: Option[MemBusFilter] = None
  val dbusObservers = mutable.ArrayBuffer[MemBusObserver]()

  def dummyConnect(bus: MemBus) = {
    bus.cmd.valid := False
    bus.cmd.payload.assignDontCare()
    bus.rsp.ready := False
  }

  def setupIBus(): Unit = {
    pipeline plug new Area {
      externalIBus = master(new MemBus(config.ibusConfig)).setName("ibus")

      if (internalIBus != null) {
        if (ibusFilter.isEmpty) {
          ibusFilter = Some((_, iibus, eibus) => {
            iibus <> eibus
          })
        }
        ibusFilter.foreach(_(null, internalIBus, externalIBus))
      } else {
        internalIBus = externalIBus
        dummyConnect(internalIBus)
      }
    }
  }

  override def finish(): Unit = {
    setupIBus()

    // DBUS
    if (dbusFilter.isEmpty) {
      dbusFilter = Some((_, idbus, edbus) => {
        idbus <> edbus
      })
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

  override def getDBusStages: Seq[Stage] = {
    internalReadDBusStages.filter(_ != null).distinct
  }

  override def filterDBus(filter: MemBusFilter): Unit = {
    assert(dbusFilter.isEmpty)
    dbusFilter = Some(filter)
  }

  override def filterIBus(filter: MemBusFilter): Unit = {
    assert(ibusFilter.isEmpty)
    ibusFilter = Some(filter)
  }

  override def observeDBus(observer: MemBusObserver): Unit = {
    dbusObservers += observer
  }
}
