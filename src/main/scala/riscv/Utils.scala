package riscv

import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

trait DynBundleAccess[KeyType] {
  def element(key: KeyType): Data

  def elementAs[T <: Data](key: KeyType): T = {
    element(key).asInstanceOf[T]
  }
}

class DynBundle[KeyType] {
  private val elementsMap = mutable.Map[KeyType, Data]()

  val keys: Iterable[KeyType] = elementsMap.keys

  def addElement[T <: Data](key: KeyType, hardType: HardType[T]) = {
    val data = hardType()
    elementsMap(key) = data
  }

  def createBundle: Bundle with DynBundleAccess[KeyType] = {
    class NewBundle extends Bundle with DynBundleAccess[KeyType] {
      private val elementsMap = DynBundle.this.elementsMap.map {case (key, data) =>
        val clonedData = cloneOf(data)
        clonedData.parent = this

        if (OwnableRef.proposal(clonedData, this)) {
          clonedData.setPartialName(key.toString, Nameable.DATAMODEL_WEAK)
        }

        (key, clonedData)
      }

      override val elements: ArrayBuffer[(String, Data)] = elementsMap.map{
        case (key, data) => (key.toString, data) }.toSeq.to[mutable.ArrayBuffer]

      override def element(key: KeyType): Data = {
        elementsMap(key)
      }

      override def clone(): Bundle = {
        new NewBundle
      }
    }

    new NewBundle
  }
}
