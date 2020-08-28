package riscv.plugins.capabilities

import spinal.core._

case class Permissions() extends Bundle {
  val execute = Bool()
  val load = Bool()
  val store = Bool()
  val loadCapability = Bool()
  val storeCapability = Bool()
  val accessSystemRegisters = Bool()

  def allowAll() = setAll(True)
  def allowNone() = setAll(False)

  private def setAll(value: Bool) = {
    for ((_, element) <- elements) {
      element := value
    }
  }

  def asIsaBits: Bits = {
    B"0" ## accessSystemRegisters ## B"0000" ## storeCapability ##
      loadCapability ## store ## load ## execute ## B"0" resized
  }

  def assignFromIsaBits(bits: Bits): Unit = {
    execute := bits(1)
    load := bits(2)
    store := bits(3)
    loadCapability := bits(4)
    storeCapability := bits(5)
    accessSystemRegisters := bits(10)
  }
}

trait Capability {
  def tag: Bool
  def base: UInt
  def length: UInt
  def offset: UInt
  def perms: Permissions

  def hasOffset: Boolean = true

  def top: UInt = base + length
  def address: UInt = base + offset

  def assignFrom(other: Capability): this.type = {
    tag := other.tag
    base := other.base
    length := other.length
    perms := other.perms

    if (hasOffset && other.hasOffset) {
      offset := other.offset
    }

    this
  }

  def assignRoot(): this.type = {
    tag := True
    base := 0
    length := U"32'hffffffff"
    perms.allowAll()

    if (hasOffset) {
      offset := 0
    }

    this
  }

  def assignNull(): this.type = {
    tag := False
    base := 0
    length := 0
    perms.allowNone()

    if (hasOffset) {
      offset := 0
    }

    this
  }
}

class PackedCapabilityFields(hasOffset: Boolean = true)
                            (implicit context: Context) extends Bundle {
  private val xlen = context.config.xlen
  val base = UInt(xlen bits)
  val length = UInt(xlen bits)
  val offset = if (hasOffset) UInt(xlen bits) else null
  val perms = Permissions()
}

object PackedCapabilityFields {
  def getBitsWidth(hasOffset: Boolean = true)(implicit context: Context): Int = {
    val fields = new PackedCapabilityFields(hasOffset)
    fields.getBitsWidth
  }
}

case class PackedCapability(override val hasOffset: Boolean = true)
                           (implicit context: Context)
  extends PackedCapabilityFields(hasOffset) with Capability {
  override val tag = Bool()
}

object PackedCapability {
  def Root(hasOffset: Boolean = true)(implicit context: Context): PackedCapability = {
    PackedCapability(hasOffset).assignRoot()
  }

  def Null(hasOffset: Boolean = true)(implicit context: Context): PackedCapability = {
    PackedCapability(hasOffset).assignNull()
  }
}

case class RegCapability(implicit context: Context)
  extends PackedCapabilityFields with Capability {
  private val padding = Bits(context.clen - PackedCapabilityFields.getBitsWidth() bits)
  override val tag = Bool()

  assert(getBitsWidth == context.clen + 1)
}

object RegCapability {
  def Null(implicit context: Context): RegCapability = {
    val cap = RegCapability().assignNull()
    cap.padding := 0
    cap
  }

  def Root(implicit context: Context): RegCapability = {
    val cap = RegCapability().assignRoot()
    cap.padding := 0
    cap
  }
}
