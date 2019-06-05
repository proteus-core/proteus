package riscv.soc

import riscv._

import spinal.core._
import spinal.lib.misc.HexTools

class Soc(pipeline: Pipeline,
          memMap: Seq[MemorySegment],
          imemHexPath: String = "")(implicit config: Config) {
  if (!imemHexPath.isEmpty) {
    val mems = memMap.filter(segment => {
      segment match {
        case _: MemSegment => true
        case _ => false
      }
    })

    assert(mems.size == 1)
    val mem = mems(0).asInstanceOf[MemSegment].mem
    HexTools.initRam(mem, imemHexPath, mems(0).start)
  }

  val memMapper = new MemoryMapper(memMap)
  val dbus = pipeline.getService[DBusService].getDBus
  dbus <> memMapper.dbus
  val ibus = pipeline.getService[IBusService].getIBus
  ibus <> memMapper.ibus
}
