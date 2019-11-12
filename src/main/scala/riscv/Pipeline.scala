package riscv

import spinal.core._

import scala.collection.mutable
import scala.reflect.ClassTag

trait Pipeline {
  private val plugins = mutable.ArrayBuffer[Plugin[this.type]]()

  def config: Config
  def data: StandardPipelineData

  // NOTE This is deliberately not just called "component" because that causes a
  // (silent) name-clash with one of the traits implemented by Component.
  def pipelineComponent: Component
  def retirementStage: Stage

  def addPlugin(plugin: Plugin[this.type]): Unit = {
    plugins += plugin
  }

  def addPlugins(plugins: Seq[Plugin[this.type]]): Unit = {
    this.plugins ++= plugins
  }

  def build() {
    pipelineComponent.rework {
      init()
    }

    plugins.foreach(_.setup(this))
    plugins.foreach(_.build(this))

    pipelineComponent.rework {
      connectStages()
    }

    plugins.foreach(_.finish(this))
  }

  protected def init(): Unit
  protected def connectStages(): Unit

  def getService[T](implicit tag: ClassTag[T]): T = {
    val services = plugins.filter(_ match {
      case _: T => true
      case _    => false
    })

    assert(services.length == 1)
    services.head.asInstanceOf[T]
  }

  def hasService[T](implicit tag: ClassTag[T]): Boolean = {
    plugins.exists(_ match {
      case _: T => true
      case _    => false
    })
  }

  def getImplementedExtensions: Seq[Extension] = {
    Extension(config.baseIsa) +: plugins.flatMap(_.getImplementedExtensions)
  }
}
