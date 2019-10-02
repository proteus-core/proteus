package riscv

import spinal.core._

abstract class Plugin[-PipelineT <: Pipeline](implicit val config: Config) {
  def getName = getClass.getSimpleName.replace("$","")
  def setup(pipeline: PipelineT): Unit = ()
  def build(pipeline: PipelineT): Unit = ()
  def finish(pipeline: PipelineT): Unit = ()

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
