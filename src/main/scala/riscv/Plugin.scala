package riscv

import spinal.core._

trait Plugin {
  def getName = getClass.getSimpleName.replace("$","")
  def setup(pipeline: Pipeline, config: Config): Unit = ()
  def build(pipeline: Pipeline, config: Config): Unit

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
}
