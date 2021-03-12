package riscv.plugins.cheri

import riscv._

case class Context(pipeline: Pipeline)(implicit val config: Config) {
  val clen = 4 * config.xlen
  val otypeLen = 12
  val maxOtype = (1 << otypeLen) - 17
  val data = new GlobalPipelineData(pipeline)(this)
}
