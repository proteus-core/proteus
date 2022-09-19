package riscv

import spinal.core._

abstract class Plugin[-PipelineT <: Pipeline] {
  protected[this] var pipeline: PipelineT = _
  implicit def config = pipeline.config

  def setPipeline(pipeline: PipelineT): Unit = {
    assert(this.pipeline == null)
    this.pipeline = pipeline
  }

  def getName = getClass.getSimpleName.replace("$", "")
  def setup(): Unit = ()
  def build(): Unit = ()
  def finish(): Unit = ()

  implicit class PlugComponent(component: Component) {
    def plug[T](logic: => T): T = component.rework {
      val result = logic

      result match {
        case component: Component => component.setName(getName)
        case area: Area => area.setName(getName).reflectNames()
        case _ =>
      }

      result
    }
  }

  implicit class PlugPipeline(pipeline: Pipeline) {
    def plug[T](logic: => T): T = pipeline.pipelineComponent plug logic
  }

  def getImplementedExtensions: Seq[Extension] = Seq()
  implicit def charToExtension(char: Char) = Extension(char)
}
