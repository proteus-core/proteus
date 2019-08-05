package riscv.soc

import riscv._

import spinal.core._

class Soc(pipeline: Pipeline,
          memMap: Seq[MemorySegment])(implicit config: Config) {
  val memMapper = new MemoryMapper(memMap)
  val memService = pipeline.getService[MemoryService]
  val dbus = memService.getExternalDBus
  dbus <> memMapper.dbus
  val ibus = memService.getExternalIBus
  ibus <> memMapper.ibus
}
