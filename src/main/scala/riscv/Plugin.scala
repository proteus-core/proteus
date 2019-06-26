package riscv

import spinal.core._

abstract class Plugin(implicit val config: Config) {
  def getName = getClass.getSimpleName.replace("$","")
  def setup(pipeline: Pipeline): Unit = ()
  def build(pipeline: Pipeline): Unit = ()
  def finish(pipeline: Pipeline): Unit = ()

  implicit class Plug(component: Component) {
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

  def getImplementedExtensions: Seq[Extension] = Seq()
  implicit def charToExtension(char: Char) = Extension(char)
}
