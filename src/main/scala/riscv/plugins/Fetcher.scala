package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib.misc.HexTools

import java.io.File

class Fetcher(imemHexPath: String) extends Plugin {
  override def build(pipeline: Pipeline, config: Config): Unit = {
    val f = pipeline.fetch plug new Area {
      import pipeline.fetch._

      val imem = Mem(UInt(config.xlen bits), new File(imemHexPath).length() / 4)
      HexTools.initRam(imem, imemHexPath, 0)

      val pc = Reg(UInt(config.xlen bits)).init(0)

      when (arbitration.isValid) {
        pc := pc + 4
      }

      output(pipeline.data.PC) := pc
      output(pipeline.data.IR) := imem((pc >> 2).resized)
    }
  }
}
