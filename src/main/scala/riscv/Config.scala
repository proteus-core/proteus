package riscv

import spinal.core._
import riscv.plugins._

sealed trait BaseIsa {
  val xlen: Int
  val numRegs: Int
}

object BaseIsa {
  case object RV32I extends BaseIsa {
    override val xlen = 32
    override val numRegs = 32
  }
  case object RV32E extends BaseIsa {
    override val xlen = 32
    override val numRegs = 16
  }
  case object RV64I extends BaseIsa {
    override val xlen = 64
    override val numRegs = 32
  }
}

sealed trait PrefetcherType {

  /** Whether the prefetcher requires the PC to be passed to notifyLoadRequest and
    * notifyLoadResponse; when false, the PC signal will be set to null
    */
  val requiresPc: Boolean

  def create(implicit config: Config): Option[Plugin[Pipeline] with PrefetchService]
}
object PrefetcherType {
  case object None extends PrefetcherType {
    override val requiresPc = false
    override def create(implicit config: Config): Option[Plugin[Pipeline] with PrefetchService] = {
      scala.None
    }
  }
  case object SequentialInstructionPrefetcher extends PrefetcherType {
    override val requiresPc = false
    override def create(implicit config: Config): Option[Plugin[Pipeline] with PrefetchService] = {
      Some(new memory.SequentialInstructionPrefetcher)
    }
  }
}

sealed trait PluginType {
  def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline]
}
object PluginType {
  case object Fence extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      new Fence(fenceStages)
    }
  }
  case object Marker extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      new Marker
    }
  }
  case object RiscvFormal extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      new RiscvFormal
    }
  }
  case object DataSpeculationTracking extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      new DataSpeculationTracking
    }
  }
  case object ControlSpeculationTracking extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      config match {
        case dynamicPipelineConfig: DynamicPipelineConfig =>
          new ControlSpeculationTracking()(dynamicPipelineConfig).asInstanceOf[Plugin[Pipeline]]
        case other =>
          throw new IllegalArgumentException(
            "ControlSpeculationTracking requires a DynamicPipelineConfig"
          )
      }
    }
  }
  case object PipelineTaintTracking extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      new PipelineTaintTracking().asInstanceOf[Plugin[Pipeline]]
    }
  }
  case object RegisterFence extends PluginType {
    override def create(fenceStages: Set[Stage])(implicit config: Config): Plugin[Pipeline] = {
      new RegisterFence().asInstanceOf[Plugin[Pipeline]]
    }
  }
}

case class CacheConfig(
    sets: Int,
    ways: Int,
    prefetcher: PrefetcherType = PrefetcherType.None,
    maxPrefetches: Int = 1,
    delay: Int = 0
)

case class MemBusConfig(
    addressWidth: Int,
    idWidth: Int,
    dataWidth: Int,
    readWrite: Boolean = true,
    includePcWire: Boolean = false
) {
  def byte2WordAddress(ba: UInt): UInt = ba(dataWidth - 1 downto log2Up(dataWidth / 8))
  def word2ByteAddress(wa: UInt): UInt = wa << log2Up(dataWidth / 8)
}

sealed abstract class Config(
    val isa: BaseIsa,
    val memBusWidth: Int,
    val idWidth: Int,
    val extraDbusReadDelay: Int,
    val applyDelayToIBus: Boolean,
    val iCaches: Seq[CacheConfig],
    val dCaches: Seq[CacheConfig],
    val stlSpec: Boolean,
    val extraPlugins: Seq[PluginType],
    val debug: Boolean
) {
  val internalIBusConfig = MemBusConfig(
    addressWidth = isa.xlen,
    idWidth = 2,
    dataWidth = memBusWidth,
    readWrite = false,
    includePcWire = iCaches.exists(_.prefetcher.requiresPc)
  )

  val externalIBusConfig = MemBusConfig(
    addressWidth = isa.xlen,
    idWidth = 2,
    dataWidth = memBusWidth,
    readWrite = false
  )

  val internalReadDBusConfig = MemBusConfig(
    addressWidth = isa.xlen,
    idWidth = idWidth,
    dataWidth = memBusWidth,
    readWrite = false,
    includePcWire = dCaches.exists(_.prefetcher.requiresPc)
  )

  val internalDBusConfig = MemBusConfig(
    addressWidth = isa.xlen,
    idWidth = idWidth,
    dataWidth = memBusWidth,
    includePcWire = dCaches.exists(_.prefetcher.requiresPc)
  )

  val externalDBusConfig = MemBusConfig(
    addressWidth = isa.xlen,
    idWidth = idWidth,
    dataWidth = memBusWidth
  )

  override def toString: String =
    s"""${getClass.getSimpleName}(
       |  isa=$isa,
       |  memBusWidth=$memBusWidth,
       |  idWidth=$idWidth,
       |  extraDbusReadDelay=$extraDbusReadDelay,
       |  applyDelayToIBus=$applyDelayToIBus,
       |  iCaches=$iCaches,
       |  dCaches=$dCaches,
       |  stlSpec=$stlSpec,
       |  extraPlugins=$extraPlugins,
       |  debug=$debug
       |)""".stripMargin
}

class StaticPipelineConfig(
    isa: BaseIsa = BaseIsa.RV32I,
    memBusWidth: Int = 128,
    idWidth: Int = log2Up(4),
    extraDbusReadDelay: Int = 32,
    applyDelayToIBus: Boolean = false,
    iCaches: Seq[CacheConfig] = Seq(
      CacheConfig(
        sets = 2,
        ways = 2,
        prefetcher = PrefetcherType.SequentialInstructionPrefetcher
      )
    ),
    dCaches: Seq[CacheConfig] = Seq(CacheConfig(sets = 8, ways = 2)),
    extraPlugins: Seq[PluginType] = Seq(PluginType.Fence, PluginType.Marker),
    debug: Boolean = true
) extends Config(
      isa = isa,
      memBusWidth = memBusWidth,
      idWidth = idWidth,
      extraDbusReadDelay = extraDbusReadDelay,
      applyDelayToIBus = applyDelayToIBus,
      iCaches = iCaches,
      dCaches = dCaches,
      stlSpec = false,
      extraPlugins = extraPlugins,
      debug = debug
    )

class DynamicPipelineConfig(
    isa: BaseIsa = BaseIsa.RV32I,
    memBusWidth: Int = 128,
    extraDbusReadDelay: Int = 32,
    applyDelayToIBus: Boolean = false,
    iCaches: Seq[CacheConfig] = Seq(
      CacheConfig(
        sets = 2,
        ways = 2,
        prefetcher = PrefetcherType.SequentialInstructionPrefetcher
      )
    ),
    dCaches: Seq[CacheConfig] = Seq(CacheConfig(sets = 8, ways = 2)),
    val robEntries: Int = 32,
    val parallelAlus: Int = 8,
    val parallelMulDivs: Int = 2,
    val parallelLoads: Int = 3,
    val addressBasedPsf: Boolean = true,
    val addressBasedSsb: Boolean = true,
    stlSpec: Boolean = true,
    extraPlugins: Seq[PluginType] = Seq(
      PluginType.Fence,
      PluginType.Marker,
      PluginType.DataSpeculationTracking,
      PluginType.ControlSpeculationTracking,
      PluginType.PipelineTaintTracking
    ),
    debug: Boolean = true
) extends Config(
      isa = isa,
      memBusWidth = memBusWidth,
      idWidth = log2Up(parallelLoads + 1),
      extraDbusReadDelay = extraDbusReadDelay,
      applyDelayToIBus = applyDelayToIBus,
      iCaches = iCaches,
      dCaches = dCaches,
      stlSpec = stlSpec,
      extraPlugins = extraPlugins,
      debug = debug
    ) {
  override def toString: String =
    s"""${super.toString.dropRight(1)}
       |  robEntries=$robEntries,
       |  parallelAlus=$parallelAlus,
       |  parallelMulDivs=$parallelMulDivs,
       |  parallelLoads=$parallelLoads,
       |  addressBasedPsf=$addressBasedPsf,
       |  addressBasedSsb=$addressBasedSsb
       |)""".stripMargin
}

object Config {
  private def getIsa(isa: String): BaseIsa = {
    isa match {
      case "RV32I" => BaseIsa.RV32I
      case "RV32E" => BaseIsa.RV32E
      case "RV64I" => BaseIsa.RV64I
      case other => throw new IllegalArgumentException(s"Unknown ISA: $other")
    }
  }

  def apply(jsonString: String): Config = {
    val jsonObject = ujson.read(jsonString)

    def parseBool(jsonObject: ujson.Value, key: String, default: Boolean): Boolean = {
      jsonObject.obj.get(key).map(_.bool).getOrElse(default)
    }

    def parseInt(jsonObject: ujson.Value, key: String, default: Int): Int = {
      jsonObject.obj.get(key).map(_.num.toInt).getOrElse(default)
    }

    def parseIsa(jsonObject: ujson.Value, key: String, default: BaseIsa): BaseIsa = {
      jsonObject.obj.get(key).map(_.str) match {
        case Some(isa) => getIsa(isa)
        case None => default
      }
    }

    def parsePrefetcher(
        jsonObject: ujson.Value,
        key: String,
        default: PrefetcherType
    ): PrefetcherType = {
      jsonObject.obj.get(key).map(_.str) match {
        case Some(prefetcherType) =>
          prefetcherType match {
            case "None" => PrefetcherType.None
            case "SequentialInstructionPrefetcher" => PrefetcherType.SequentialInstructionPrefetcher
            case other => throw new IllegalArgumentException(s"Unknown prefetcher: $other")
          }
        case None => default
      }
    }

    def parseCaches(
        jsonObject: ujson.Value,
        key: String,
        default: CacheConfig
    ): Seq[CacheConfig] = {
      jsonObject.obj.get(key).map(_.arr) match {
        case Some(caches) => caches.map(cache => parseCache(cache, default))
        case None => Seq(default)
      }
    }

    def parseCache(cache: ujson.Value, default: CacheConfig): CacheConfig = {
      CacheConfig(
        sets = parseInt(cache, "sets", default.sets),
        ways = parseInt(cache, "ways", default.ways),
        prefetcher = parsePrefetcher(cache, "prefetcher", default.prefetcher),
        maxPrefetches = parseInt(cache, "maxPrefetches", default.maxPrefetches),
        delay = parseInt(cache, "delay", default.delay)
      )
    }

    def parseExtraPlugins(
        jsonObject: ujson.Value,
        key: String,
        default: Seq[PluginType]
    ): Seq[PluginType] = {
      jsonObject.obj.get(key).map(_.arr) match {
        case Some(plugins) =>
          plugins.map { plugin =>
            plugin.str match {
              case "Fence" => PluginType.Fence
              case "Marker" => PluginType.Marker
              case "RiscvFormal" => PluginType.RiscvFormal
              case "DataSpeculationTracking" => PluginType.DataSpeculationTracking
              case "ControlSpeculationTracking" => PluginType.ControlSpeculationTracking
              case "PipelineTaintTracking" => PluginType.PipelineTaintTracking
              case "RegisterFence" => PluginType.RegisterFence
              case other => throw new IllegalArgumentException(s"Unknown plugin: $other")
            }
          }
        case None => default
      }
    }

    jsonObject("pipeline").str match {
      case "Static" => {
        val default: StaticPipelineConfig = new StaticPipelineConfig()
        new StaticPipelineConfig(
          isa = parseIsa(jsonObject, "isa", default.isa),
          memBusWidth = parseInt(jsonObject, "memBusWidth", default.memBusWidth),
          idWidth = parseInt(jsonObject, "idWidth", default.idWidth),
          extraDbusReadDelay =
            parseInt(jsonObject, "extraDbusReadDelay", default.extraDbusReadDelay),
          applyDelayToIBus = parseBool(jsonObject, "applyDelayToIBus", default.applyDelayToIBus),
          iCaches = parseCaches(jsonObject, "iCaches", default.iCaches.head),
          dCaches = parseCaches(jsonObject, "dCaches", default.dCaches.head),
          extraPlugins = parseExtraPlugins(jsonObject, "extraPlugins", default.extraPlugins),
          debug = parseBool(jsonObject, "debug", default.debug)
        )
      }
      case "Dynamic" => {
        val default: DynamicPipelineConfig = new DynamicPipelineConfig()
        new DynamicPipelineConfig(
          isa = parseIsa(jsonObject, "isa", default.isa),
          memBusWidth = parseInt(jsonObject, "memBusWidth", default.memBusWidth),
          extraDbusReadDelay =
            parseInt(jsonObject, "extraDbusReadDelay", default.extraDbusReadDelay),
          applyDelayToIBus = parseBool(jsonObject, "applyDelayToIBus", default.applyDelayToIBus),
          iCaches = parseCaches(jsonObject, "iCaches", default.iCaches.head),
          dCaches = parseCaches(jsonObject, "dCaches", default.dCaches.head),
          robEntries = parseInt(jsonObject, "robEntries", default.robEntries),
          parallelAlus = parseInt(jsonObject, "parallelAlus", default.parallelAlus),
          parallelMulDivs = parseInt(jsonObject, "parallelMulDivs", default.parallelMulDivs),
          parallelLoads = parseInt(jsonObject, "parallelLoads", default.parallelLoads),
          addressBasedPsf = parseBool(jsonObject, "addressBasedPsf", default.addressBasedPsf),
          addressBasedSsb = parseBool(jsonObject, "addressBasedSsb", default.addressBasedSsb),
          stlSpec = parseBool(jsonObject, "stlSpec", default.stlSpec),
          extraPlugins = parseExtraPlugins(jsonObject, "extraPlugins", default.extraPlugins),
          debug = parseBool(jsonObject, "debug", default.debug)
        )
      }
      case other => throw new IllegalArgumentException(s"Unknown pipeline: $other")
    }
  }

  def apply(pipeline: String, isa: String): Config = {
    pipeline match {
      case "Static" => new StaticPipelineConfig(isa = getIsa(isa))
      case "Dynamic" => new DynamicPipelineConfig(isa = getIsa(isa))
      case other => throw new IllegalArgumentException(s"Unknown pipeline: $other")
    }
  }
}
