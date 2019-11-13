package riscv

import spinal.core._

import scala.reflect.ClassTag

trait DynamicPipeline extends Pipeline {
  val issuePipeline: StaticPipeline

  override val retirementStage: Stage = null

  override def build(): Unit = {
    issuePipeline.build()
    super.build()
  }

  override def init(): Unit = {
  }

  override def connectStages(): Unit = {
  }

  override def getService[T](implicit tag: ClassTag[T]): T = {
    if (super.hasService[T]) {
      super.getService[T]
    } else {
      issuePipeline.getService[T]
    }
  }

  override def hasService[T](implicit tag: ClassTag[T]): Boolean = {
    super.hasService[T] || issuePipeline.hasService[T]
  }
}
