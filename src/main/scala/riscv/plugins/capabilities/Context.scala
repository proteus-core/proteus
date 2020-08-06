package riscv.plugins.capabilities

import riscv._

case class Context(pipeline: Pipeline)(implicit val config: Config) {
  val data = new GlobalPipelineData(pipeline)(this)
}
