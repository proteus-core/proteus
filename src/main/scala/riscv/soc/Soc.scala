package riscv.soc

import riscv._

import spinal.core._

class Soc(pipeline: Pipeline,
          memMap: Seq[MemorySegment])(implicit config: Config) {
  val memMapper = new MemoryMapper(memMap)
  val dbus = pipeline.getService[DBusService].getDBus
  dbus <> memMapper.dbus
  val ibus = pipeline.getService[IBusService].getIBus
  ibus <> memMapper.ibus
}
