package riscv

import riscv.plugins._
import riscv.soc._
import spinal.core._
import spinal.core.sim._

object createStaticPipeline {
  def apply(
      disablePipelining: Boolean = false,
      extraPlugins: Seq[Plugin[StaticPipeline]] = Seq(),
      build: Boolean = true
  )(implicit conf: StaticPipelineConfig): StaticPipeline = {
    import riscv.plugins.scheduling.static._

    val pipeline = new Component with StaticPipeline {
      setDefinitionName("Pipeline")

      val fetch = new Stage("IF")
      val decode = new Stage("ID")
      val execute = new Stage("EX")
      val memory = new Stage("MEM")
      val writeback = new Stage("WB")

      override val stages = Seq(fetch, decode, execute, memory, writeback)
      override val passThroughStage: Stage = execute
      override val config: Config = conf
      override val data: StandardPipelineData = new StandardPipelineData(conf)
      override val pipelineComponent: Component = this
    }

    if (disablePipelining) {
      pipeline.addPlugin(new NoPipeliningScheduler)
    } else {
      pipeline.addPlugins(
        Seq(
          new Scheduler,
          new DataHazardResolver(firstRsReadStage = pipeline.execute)
        )
      )
    }

    val backbone = new memory.StaticMemoryBackbone

    val iPrefetchers: Seq[Option[Plugin[Pipeline] with PrefetchService]] =
      conf.iCaches.map(_.prefetcher.create)
    val dPrefetchers: Seq[Option[Plugin[Pipeline] with PrefetchService]] =
      conf.dCaches.map(_.prefetcher.create)
    val iCaches: Seq[Cache] = conf.iCaches.zipWithIndex.map { case (iCache, i) =>
      new Cache(
        sets = iCache.sets,
        ways = iCache.ways,
        busFilter = backbone.filterIBus,
        prefetcher = iPrefetchers(i),
        maxPrefetches = iCache.maxPrefetches,
        cacheable = (_ => True),
        delay = iCache.delay
      )
    }
    val dCaches: Seq[Cache] = conf.dCaches.zipWithIndex.map { case (dCache, i) =>
      new Cache(
        sets = dCache.sets,
        ways = dCache.ways,
        busFilter = backbone.filterDBus,
        prefetcher = dPrefetchers(i),
        maxPrefetches = dCache.maxPrefetches,
        cacheable = (_ >= 0x80000000L),
        delay = dCache.delay
      )
    }

    pipeline.addPlugins(
      Seq(
        backbone,
        new memory.Fetcher(pipeline.fetch),
        new Decoder(pipeline.decode),
        new RegisterFileAccessor(pipeline.decode, pipeline.writeback),
        new IntAlu(Set(pipeline.execute)),
        new Shifter(Set(pipeline.execute)),
        new Conditional(Set(pipeline.execute)),
        new memory.Lsu(Set(pipeline.memory), Seq(pipeline.memory), pipeline.memory),
        new BranchUnit(Set(pipeline.execute)),
        new PcManager(0x80000000L),
        new BranchTargetPredictor(pipeline.fetch, pipeline.execute, 8, conf.isa.xlen)
      ) ++
        iPrefetchers.flatten ++
        dPrefetchers.flatten ++
        iCaches ++
        dCaches ++
        Seq(
          new CsrFile(pipeline.writeback, pipeline.writeback), // TODO: ugly
          new Timers,
          new MachineMode(pipeline.execute),
          new TrapHandler(pipeline.writeback),
          new TrapStageInvalidator,
          new Interrupts(pipeline.writeback),
          new MulDiv(Set(pipeline.execute)),
          new Fence(Set(pipeline.execute)),
          new Marker,
          new PMP
        ) ++ extraPlugins
    )

    if (build) {
      pipeline.build()
    }

    pipeline
  }
}

object SoC {
  def static(
      ramType: RamType,
      extraDbusReadDelay: Int = 0,
      applyDelayToIBus: Boolean = false,
      extraPlugins: Seq[Plugin[StaticPipeline]] = Seq()
  )(implicit config: StaticPipelineConfig): SoC[StaticPipelineConfig] = {
    new SoC(
      ramType,
      config => createStaticPipeline(extraPlugins = extraPlugins)(config),
      extraDbusReadDelay,
      applyDelayToIBus
    )
  }

  def dynamic(
      ramType: RamType,
      extraDbusReadDelay: Int = 0,
      applyDelayToIBus: Boolean = false,
      extraPlugins: Seq[Plugin[DynamicPipeline]] = Seq()
  )(implicit config: DynamicPipelineConfig): SoC[DynamicPipelineConfig] = {
    new SoC(
      ramType,
      config => createDynamicPipeline(extraPlugins = extraPlugins)(config),
      extraDbusReadDelay,
      applyDelayToIBus
    )
  }
}

object Core {
  def main(args: Array[String]) {
    implicit val config = new StaticPipelineConfig(BaseIsa.RV32I)
    SpinalVerilog(SoC.static(RamType.OnChipRam(1 GiB, args.headOption)))
  }
}

object CoreSim {
  def main(args: Array[String]) {
    implicit val config = new StaticPipelineConfig(BaseIsa.RV32I)
    SimConfig.withWave.compile(SoC.static(RamType.OnChipRam(1 GiB, Some(args(0))))).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      val byteDevSim = new riscv.sim.StdioByteDev(dut.io.byteDev)

      var done = false

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.io.charOut.valid.toBoolean) {
          val char = dut.io.charOut.payload.toInt.toChar

          if (char == 4) {
            println("Simulation halted by software")
            done = true
          } else {
            print(char)
          }
        }

        byteDevSim.eval()
      }
    }
  }
}

class CoreFormal extends Component {
  setDefinitionName("Core")

  implicit val config = new StaticPipelineConfig(BaseIsa.RV32I)
  val pipeline = createStaticPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreFormal)
  }
}

object CoreTestSim {
  def main(args: Array[String]) {
    implicit val config = new StaticPipelineConfig(BaseIsa.RV32I)
    var mainResult = 0

    SimConfig.withWave.compile(SoC.static(RamType.OnChipRam(1 GiB, Some(args(0))))).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      var done = false

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.io.testDev.valid.toBoolean) {
          val result = dut.io.testDev.payload.toBigInt

          if (result == 0) {
            println("All tests passed")
          } else {
            println(s"Test $result failed")
            mainResult = 1
          }

          done = true
        }
      }
    }

    sys.exit(mainResult)
  }
}

object createDynamicPipeline {
  def apply(extraPlugins: Seq[Plugin[DynamicPipeline]] = Seq(), build: Boolean = true)(implicit
      conf: DynamicPipelineConfig
  ): DynamicPipeline = {
    val pipeline = new Component with DynamicPipeline {
      setDefinitionName("Pipeline")

      override val config = conf
      override val data = new StandardPipelineData(conf)
      override val pipelineComponent = this

      val dynamicPipeline: DynamicPipeline = this

      val fetch = new Stage("IF").setName("fetch")
      val decode = new Stage("ID").setName("decode")

      override val issuePipeline = new StaticPipeline {
        override val parentPipeline = dynamicPipeline
        override val stages = Seq(fetch, decode)
        override val config = dynamicPipeline.config
        override val data = dynamicPipeline.data
        override val pipelineComponent = dynamicPipeline.pipelineComponent
        override val passThroughStage: Stage = decode // dummy
      }

      val intAlus = (0 until config.parallelAlus).map(n => new Stage("EX_ALU" + n))
      val intMuls = (0 until config.parallelMulDivs).map(n => new Stage("EX_MUL" + n))
      val loadStages = (0 until config.parallelLoads).map(n => new Stage("LOAD" + n))

      override val passThroughStage: Stage = intAlus.last
      override val rsStages: Seq[Stage] = intAlus ++ intMuls
      override val retirementStage = new Stage("RET")
      override val parallelStages: Seq[Stage] = rsStages ++ loadStages
      override val stages = issuePipeline.stages ++ parallelStages :+ retirementStage
      override val backbone = new memory.DynamicMemoryBackbone
    }

    pipeline.issuePipeline.addPlugins(
      Seq(
        new scheduling.static.Scheduler(canStallExternally = true),
        new scheduling.static.PcManager(0x80000000L),
        pipeline.backbone,
        new memory.Fetcher(pipeline.fetch),
        new PMP
      )
    )

    val iPrefetchers: Seq[Option[Plugin[Pipeline] with PrefetchService]] =
      conf.iCaches.map(_.prefetcher.create)
    val dPrefetchers: Seq[Option[Plugin[Pipeline] with PrefetchService]] =
      conf.dCaches.map(_.prefetcher.create)
    val iCaches: Seq[Cache] = conf.iCaches.zipWithIndex.map { case (iCache, i) =>
      new Cache(
        sets = iCache.sets,
        ways = iCache.ways,
        busFilter = pipeline.backbone.filterIBus,
        prefetcher = iPrefetchers(i),
        maxPrefetches = iCache.maxPrefetches,
        cacheable = (_ => True),
        delay = iCache.delay
      )
    }
    val dCaches: Seq[Cache] = conf.dCaches.zipWithIndex.map { case (dCache, i) =>
      new Cache(
        sets = dCache.sets,
        ways = dCache.ways,
        busFilter = pipeline.backbone.filterDBus,
        prefetcher = dPrefetchers(i),
        maxPrefetches = dCache.maxPrefetches,
        cacheable = (_ >= 0x80000000L),
        delay = dCache.delay
      )
    }

    pipeline.addPlugins(
      Seq(
        new Decoder(pipeline.decode), // TODO: ugly alert!!
        new scheduling.dynamic.Scheduler,
        new scheduling.dynamic.PcManager,
        new RegisterFileAccessor(
          // FIXME this works since there is no delay between ID and dispatch. It would probably be
          // safer to create an explicit dispatch stage in the dynamic pipeline and read the registers
          // there. It could still be zero-delay of course.
          readStage = pipeline.decode,
          writeStage = pipeline.retirementStage
        ),
        new memory.Lsu(
          pipeline.intAlus.toSet,
          pipeline.loadStages,
          pipeline.retirementStage
        ),
        new BranchTargetPredictor(
          pipeline.fetch,
          pipeline.retirementStage,
          8,
          conf.isa.xlen
        )
      ) ++
        iPrefetchers.flatten ++
        dPrefetchers.flatten ++
        iCaches ++
        dCaches ++
        Seq(
          new IntAlu(pipeline.intAlus.toSet),
          new Shifter(pipeline.intAlus.toSet),
          new MulDiv(pipeline.intMuls.toSet),
          new Conditional(pipeline.intAlus.toSet),
          new BranchUnit(pipeline.intAlus.toSet),
          new CsrFile(pipeline.retirementStage, pipeline.intAlus.last),
          new TrapHandler(pipeline.retirementStage),
          new MachineMode(pipeline.intAlus.last),
          new Interrupts(pipeline.retirementStage),
          new Timers,
          new Fence(pipeline.rsStages.toSet),
          new Marker,
          new DataSpeculationTracking,
          new ControlSpeculationTracking,
          new PipelineTaintTracking
        ) ++ extraPlugins
    )

    if (build) {
      pipeline.build()
    }

    pipeline
  }
}

object CoreDynamic {
  def main(args: Array[String]) {
    implicit val config = new DynamicPipelineConfig(BaseIsa.RV32I)
    SpinalVerilog(SoC.dynamic(RamType.OnChipRam(1 GiB, args.headOption)))
  }
}

object CoreDynamicSim {
  def main(args: Array[String]) {
    implicit val config = new DynamicPipelineConfig(BaseIsa.RV32I)
    SimConfig.withWave.compile(SoC.dynamic(RamType.OnChipRam(1 GiB, Some(args(0))))).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      var done = false
      var i = 0

      while (!done) {
        dut.clockDomain.waitSampling()

        if (dut.io.charOut.valid.toBoolean) {
          val char = dut.io.charOut.payload.toInt.toChar

          if (char == 4) {
            println("Simulation halted by software")
            done = true
          } else {
            print(char)
          }
        }

        i += 1

        if (i == 200) {
//          done = true
        }
      }
    }
  }
}

class CoreDynamicFormal extends Component {
  setDefinitionName("Core")

  implicit val config = new DynamicPipelineConfig(BaseIsa.RV32I)
  val pipeline = createDynamicPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreDynamicFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreDynamicFormal)
  }
}

/** Build a core with external memory, according to the given configuration
  *
  * The configuration can be provided through command line arguments in two ways: either specify the
  * pipeline type (Static or Dynamic) and ISA (RV32I, RV64I, etc.), or provide the path to a JSON
  * configuration file for complete control over the core's configuration
  *
  * The JSON file must contain a "pipeline" field, either "Static" or "Dynamic", followed by the
  * properties that differ from the default core configuration of `Config.scala`
  *   - A minimal example: { "pipeline": "Dynamic", "dCaches": [ { "sets": 16, "ways": 4 } ] }
  *   - More examples are available in the Proteus Ecosystem
  *
  * The core can be built with the Proteus Ecosystem as either:
  *   - make CORE=riscv.CoreExtMem PIPELINE=pipeline ISA=isa
  *     - E.g.: make CORE=riscv.CoreExtMem PIPELINE=Dynamic ISA=RV64I
  *   - make CORE=riscv.CoreExtMem CONFIG=file_path
  */
object CoreExtMem {
  def main(args: Array[String]) {
    if (args.length < 1 || args.length > 2) {
      throw new IllegalArgumentException(
        "Incorrect configuration provided: provide either the path to the JSON configuration file, " +
          "or the pipeline type and ISA as command line arguments"
      )
    }

    var config: Config = null
    if (args.length == 1) {
      val configPath: String = args(0)
      val configFile = scala.io.Source.fromFile(configPath)
      val jsonString = configFile.getLines().mkString("\n")
      configFile.close()

      config = Config(jsonString)
    } else {
      config = Config(args(0), args(1))
    }

    if (config.iCaches.length > 1) {
      throw new IllegalArgumentException(
        s"Too many instruction caches: Proteus currently supports a maximum of 1 instruction cache"
      )
    }

    config match {
      case config: StaticPipelineConfig =>
        println("Parsed config as:\n" + config.toString)
        SpinalVerilog(
          SoC.static(
            RamType.ExternalAxi4(1 GiB),
            extraDbusReadDelay = config.extraDbusReadDelay,
            applyDelayToIBus = config.applyDelayToIBus
          )(config)
        )
      case config: DynamicPipelineConfig =>
        println("Parsed config as:\n" + config.toString)
        SpinalVerilog(
          SoC.dynamic(
            RamType.ExternalAxi4(1 GiB),
            extraDbusReadDelay = config.extraDbusReadDelay,
            applyDelayToIBus = config.applyDelayToIBus
          )(config)
        )
    }
  }
}
