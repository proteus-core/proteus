package riscv.plugins.cheri

import riscv._

case class Context(pipeline: Pipeline)(implicit val config: Config) {
  val clen = 4 * config.xlen // capability length
  val otypeLen = 12 // bit width reserved for the Object Type field
  val maxOtype =
    (1 << otypeLen) - 17 // value used to know whether a capability is sealed, cf https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-951.pdf p. 78
  val data = new GlobalPipelineData(pipeline)(this)
}
