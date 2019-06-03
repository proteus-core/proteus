package riscv.soc

import riscv._

import spinal.core._
import spinal.lib.misc.HexTools

import java.io.File

class Soc(pipeline: Pipeline,
          memMap: Seq[MemorySegment],
          imemHexPath: String = "")(implicit config: Config) {
  val imem = if (!imemHexPath.isEmpty) {
    println(s"Mem size: ${new File(imemHexPath).length() / 4}")
    val imem = Mem(UInt(config.xlen bits), new File(imemHexPath).length() / 4)
    HexTools.initRam(imem, imemHexPath, 0)
    imem
  } else {
    Mem(UInt(config.xlen bits), 1024)
  }

  val ibus = pipeline.getService[IBusService].getIBus
  ibus.rdata := imem(ibus.address.resized)

  val memMapper = new MemoryMapper(memMap)
  val dbus = pipeline.getService[DBusService].getDBus
  dbus <> memMapper.bus
}
