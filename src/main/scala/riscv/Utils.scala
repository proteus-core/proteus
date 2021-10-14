package riscv

import spinal.core._
import spinal.lib._

import scala.collection.mutable

object Utils {
  def signExtend[T <: BitVector](data: T, width: Int): T = {
    val dataWidth = data.getBitsWidth
    assert(dataWidth <= width && dataWidth > 0)

    (B((width - 1 - dataWidth downto 0) -> data(dataWidth - 1)) ## data).as(data.clone().setWidth(width))
  }

  def zeroExtend[T <: BitVector](data: T, width: Int): T = {
    val dataWidth = data.getBitsWidth
    assert(dataWidth <= width && dataWidth > 0)

    (B((width - 1 - dataWidth downto 0) -> False) ## data).as(data.clone().setWidth(width))
  }

  def twosComplement(data: UInt): UInt = ~data + 1

  def delay(cycles: Int)(logic: => Unit) = {
    assert(cycles >= 0)

    val delayCounter = Counter(cycles + 1)

    when (delayCounter.willOverflowIfInc) {
      logic
    }

    delayCounter.increment()
  }

  def outsideConditionScope[T](rtl: => T): T = {
    val body = Component.current.dslBody
    body.push()
    val swapContext = body.swap()
    val ret = rtl
    body.pop()
    swapContext.appendBack()
    ret
  }
}

trait DynBundleAccess {
  def element(name: String): Data

  def elementAs[T <: Data](name: String): T = {
    element(name).asInstanceOf[T]
  }
}

class DynBundle {
  private val elementsMap = mutable.Map[String, Data]()

  def addElement[T <: Data](name: String, hardType: HardType[T]) = {
    val data = hardType()
    elementsMap(name) = data
  }

  def createBundle: Bundle with DynBundleAccess = {
    class NewBundle extends Bundle with DynBundleAccess {
      private val elementsMap = DynBundle.this.elementsMap.map {case (name, data) =>
        val clonedData = cloneOf(data)
        clonedData.parent = this

        if (OwnableRef.proposal(clonedData, this)) {
          clonedData.setPartialName(name, Nameable.DATAMODEL_WEAK)
        }

        (name, clonedData)
      }

      override val elements = elementsMap.toSeq.to[mutable.ArrayBuffer]

      override def element(name: String): Data = {
        elementsMap(name)
      }

      override def clone(): Bundle = {
        new NewBundle
      }
    }

    new NewBundle
  }
}