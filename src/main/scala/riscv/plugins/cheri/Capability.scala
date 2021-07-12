package riscv.plugins.cheri

import spinal.core._

trait Permissions {
  def execute: Bool
  def load: Bool
  def store: Bool
  def loadCapability: Bool
  def storeCapability: Bool
  def seal: Bool
  def cinvoke: Bool
  def unseal: Bool
  def accessSystemRegisters: Bool

  def setAll(value: Bool): Unit = {
    execute := value
    load := value
    store := value
    loadCapability := value
    storeCapability := value
    seal := value
    cinvoke := value
    unseal := value
    accessSystemRegisters := value
  }

  def allowAll(): Unit = setAll(True)
  def allowNone(): Unit = setAll(False)

  def asIsaBits: Bits = {
    B"0" ## accessSystemRegisters ## unseal ## cinvoke ## seal ## B"0" ##
      storeCapability ## loadCapability ## store ## load ## execute ## B"0" resized
  }

  def assignFromIsaBits(bits: Bits): Unit = {
    execute := bits(1)
    load := bits(2)
    store := bits(3)
    loadCapability := bits(4)
    storeCapability := bits(5)
    seal := bits(7)
    cinvoke := bits(8)
    unseal := bits(9)
    accessSystemRegisters := bits(10)
  }

  def assignFrom(other: Permissions): Unit = {
    execute := other.execute
    load := other.load
    store := other.store
    loadCapability := other.loadCapability
    storeCapability := other.storeCapability
    seal := other.seal
    cinvoke := other.cinvoke
    unseal := other.unseal
    accessSystemRegisters := other.accessSystemRegisters
  }
}

case class PackedPermissions() extends Bundle with Permissions {
  override val execute = Bool()
  override val load = Bool()
  override val store = Bool()
  override val loadCapability = Bool()
  override val storeCapability = Bool()
  override val seal = Bool()
  override val cinvoke = Bool()
  override val unseal = Bool()
  override val accessSystemRegisters = Bool()
}

case class ObjectType(implicit context: Context) extends Bundle {
  val value = UInt(context.otypeLen bits)

  def extendedValue: UInt = {
    val xlen = context.config.xlen
    isSealed ? value.resize(xlen bits) | UInt(xlen bits).setAll()
  }

  def unseal(): Unit = value.setAll()
  def isSealed: Bool = value <= context.maxOtype
}

trait Capability {
  def tag: Bool
  def base: UInt
  def length: UInt
  def offset: UInt
  def perms: Permissions
  def otype: ObjectType

  def hasOffset: Boolean = true

  def top: UInt = base + length
  def address: UInt = base + offset

  def isSealed: Bool = otype.isSealed

  def assignFrom(other: Capability): this.type = {
    tag := other.tag
    base := other.base
    length := other.length
    perms.assignFrom(other.perms)
    otype := other.otype

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
    otype.unseal()

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
    otype.unseal()

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
  val perms = PackedPermissions()
  val otype = ObjectType()
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

  def Root(implicit context: Context): PackedCapability = Root(true)

  def Null(hasOffset: Boolean = true)(implicit context: Context): PackedCapability = {
    PackedCapability(hasOffset).assignNull()
  }

  def Null(implicit context: Context): PackedCapability = Null(true)
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

case class MemPermissions() extends Bundle with Permissions {
  private val padding1 = B"0"
  override val execute = Bool()
  override val load = Bool()
  override val store = Bool()
  override val loadCapability = Bool()
  override val storeCapability = Bool()
  private val padding2 = B"0"
  override val seal = Bool()
  override val cinvoke = Bool()
  override val unseal = Bool()
  override val accessSystemRegisters = Bool()
  private val padding3 = B"0000"

  assert(getBitsWidth == 15)
}

case class MemCapability()(implicit context: Context) extends Bundle with Capability {
  private val xlen = context.config.xlen

  val cursor = UInt(xlen bits)
  override val base = UInt(xlen bits)
  override val length = UInt(xlen bits)
  private val padding1 = B"0"
  override val perms = MemPermissions()
  override val otype = ObjectType()
  private val padding2 = B(0, xlen - perms.getBitsWidth - otype.getBitsWidth - 1 bits)
  override val tag = Bool()

  override def offset = cursor - base

  def value: UInt = asBits.resize(context.clen).asUInt

  def assignValue(value: UInt): Unit = {
    assignFromBits(value.asBits, 0, context.clen bits)
  }

  override def assignFrom(other: Capability): this.type = {
    cursor := other.address
    base := other.base
    length := other.length
    perms.assignFrom(other.perms)
    otype := other.otype
    tag := other.tag
    this
  }

  assert(getBitsWidth == context.clen + 1,
    s"Bit width of MemCapability is ${getBitsWidth} but should be ${context.clen + 1}")
}
