package riscv.plugins.memory

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

class StaticMemoryBackbone(implicit config: Config) extends MemoryBackbone {

  override def finish(): Unit = {
    super.finish()

    pipeline plug new Area {
      externalDBus = master(new MemBus(config.dbusConfig)).setName("dbus")
      dbusFilter.foreach(_(internalWriteDBusStage, internalWriteDBus, externalDBus))
      dbusObservers.foreach(_(internalWriteDBusStage, internalWriteDBus))
    }
  }

  override def createInternalDBus(
      readStages: Seq[Stage],
      writeStage: Stage
  ): (Seq[MemBus], MemBus) = {
    assert(readStages.size == 1)
    assert(readStages.head == writeStage)

    internalWriteDBusStage = readStages.head

    internalWriteDBusStage plug new Area {
      val dbus = master(new MemBus(config.dbusConfig))
      internalWriteDBus = dbus
    }

    (Seq(internalWriteDBus), internalWriteDBus)
  }

}
