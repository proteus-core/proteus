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
  def fetchStage: Stage
  def retirementStage: Stage

  def addPlugin(plugin: Plugin[this.type]): Unit = {
    plugins += plugin
  }

  def addPlugins(plugins: Seq[Plugin[this.type]]): Unit = {
    this.plugins ++= plugins
  }

  final def initBuild(): Unit = {
    pipelineComponent.rework {
      init()
    }

    plugins.foreach(_.setPipeline(this))
  }

  final def setupPlugins(): Unit = {
    plugins.foreach(_.setup())
  }

  final def buildPlugins(): Unit = {
    plugins.foreach(_.build())
  }

  final def finishBuild(): Unit = {
    pipelineComponent.rework {
      connectStages()
    }

    plugins.foreach(_.finish())
  }

  def build(): Unit = {
    initBuild()
    setupPlugins()
    buildPlugins()
    finishBuild()
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
