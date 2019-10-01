package riscv

import spinal.core._

import scala.collection.mutable
import scala.reflect.ClassTag

abstract class Pipeline(val config: Config) extends Component {
  private val plugins = mutable.ArrayBuffer[Plugin[this.type]]()

  val data = new StandardPipelineData(config)

  val retirementStage: Stage

  def addPlugin(plugin: Plugin[this.type]): Unit = {
    plugins += plugin
  }

  def addPlugins(plugins: Seq[Plugin[this.type]]): Unit = {
    this.plugins ++= plugins
  }

  def build() {
    plugins.foreach(_.setup(this))
    plugins.foreach(_.build(this))

    rework {
      connectStages()
    }

    plugins.foreach(_.finish(this))
  }

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
