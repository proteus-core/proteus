package riscv

import riscv.plugins._
import riscv.soc._
import spinal.core._
import spinal.core.sim._

object createStaticPipeline {
  def apply(
      disablePipelining: Boolean = false,
      extraPlugins: Seq[Plugin[Pipeline]] = Seq(),
      build: Boolean = true
  )(implicit conf: Config): StaticPipeline = {
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

    pipeline.addPlugins(
      Seq(
        new StaticMemoryBackbone,
        new Fetcher(pipeline.fetch),
        new Decoder(pipeline.decode),
        new RegisterFileAccessor(pipeline.decode, pipeline.writeback),
        new IntAlu(Set(pipeline.execute)),
        new Shifter(Set(pipeline.execute)),
        new Lsu(Set(pipeline.memory), Seq(pipeline.memory), pipeline.memory),
        new BranchUnit(Set(pipeline.execute)),
        new PcManager(0x80000000L),
        new BranchTargetPredictor(pipeline.fetch, pipeline.execute, 8, conf.xlen),
        new CsrFile(pipeline.writeback, pipeline.writeback), // TODO: ugly
        new Timers,
        new MachineMode(pipeline.execute),
        new TrapHandler(pipeline.writeback),
        new TrapStageInvalidator,
        new Interrupts(pipeline.writeback),
        new MulDiv(Set(pipeline.execute))
      ) ++ extraPlugins
    )

    if (build) {
      pipeline.build()
    }

    pipeline
  }
}

object SoC {
  def static(ramType: RamType, extraDbusReadDelay: Int = 0): SoC = {
    new SoC(ramType, config => createStaticPipeline()(config), extraDbusReadDelay)
  }

  def dynamic(ramType: RamType, extraDbusReadDelay: Int = 0): SoC = {
    new SoC(ramType, config => createDynamicPipeline()(config), extraDbusReadDelay)
  }
}

object Core {
  def main(args: Array[String]) {
    SpinalVerilog(SoC.static(RamType.OnChipRam(10 MiB, args.headOption)))
  }
}

object CoreSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(SoC.static(RamType.OnChipRam(10 MiB, Some(args(0))))).doSim { dut =>
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

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createStaticPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreFormal)
  }
}

object CoreTestSim {
  def main(args: Array[String]) {
    var mainResult = 0

    SimConfig.withWave.compile(SoC.static(RamType.OnChipRam(10 MiB, Some(args(0))))).doSim { dut =>
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

object CoreExtMem {
  def main(args: Array[String]) {
    SpinalVerilog(SoC.static(RamType.ExternalAxi4(10 MiB), 32))
  }
}

object createDynamicPipeline {
  def apply(extraPlugins: Seq[Plugin[Pipeline]] = Seq(), build: Boolean = true)(implicit
      conf: Config
  ): DynamicPipeline = {
    val pipeline = new Component with DynamicPipeline {
      setDefinitionName("Pipeline")

      override val config = conf
      override val data = new StandardPipelineData(conf)
      override val pipelineComponent = this

      val dynamicPipeline: DynamicPipeline = this

      override val issuePipeline = new StaticPipeline {
        val fetch = new Stage("IF").setName("fetch")
        val decode = new Stage("ID").setName("decode")

        override val stages = Seq(fetch, decode)
        override val config = dynamicPipeline.config
        override val data = dynamicPipeline.data
        override val pipelineComponent = dynamicPipeline.pipelineComponent
        override val passThroughStage: Stage = decode // dummy
      }

      val intAlu1 = new Stage("EX_ALU1")
      val intAlu2 = new Stage("EX_ALU2")
      val intAlu3 = new Stage("EX_ALU3")
      val intAlu4 = new Stage("EX_ALU4")
      val intMul1 = new Stage("EX_MUL1")
      override val passThroughStage: Stage = intAlu1
      override val rsStages: Seq[Stage] = Seq(intAlu1, intAlu2, intAlu3, intAlu4, intMul1)
      override val loadStages: Seq[Stage] =
        Seq(new Stage("LOAD1"), new Stage("LOAD2"), new Stage("LOAD3"))
      override val retirementStage = new Stage("RET")
      override val unorderedStages: Seq[Stage] = rsStages ++ loadStages
      override val stages = issuePipeline.stages ++ unorderedStages :+ retirementStage
    }

    pipeline.issuePipeline.addPlugins(
      Seq(
        new scheduling.static.Scheduler(canStallExternally = true),
        new scheduling.static.PcManager(0x80000000L),
        new DynamicMemoryBackbone(
          pipeline.loadStages.size + 1
        ), // +1 for write stage (which also uses an ID currently)
        new Fetcher(pipeline.issuePipeline.fetch)
      )
    )

    pipeline.addPlugins(
      Seq(
        new Decoder(pipeline.issuePipeline.decode), // TODO: ugly alert!!
        new scheduling.dynamic.Scheduler,
        new scheduling.dynamic.PcManager,
        new RegisterFileAccessor(
          // FIXME this works since there is no delay between ID and dispatch. It would probably be
          // safer to create an explicit dispatch stage in the dynamic pipeline and read the registers
          // there. It could still be zero-delay of course.
          readStage = pipeline.issuePipeline.decode,
          writeStage = pipeline.retirementStage
        ),
        new Lsu(
          Set(pipeline.intAlu1, pipeline.intAlu2, pipeline.intAlu3, pipeline.intAlu4),
          pipeline.loadStages,
          pipeline.retirementStage
        ),
        new BranchTargetPredictor(
          pipeline.issuePipeline.fetch,
          pipeline.retirementStage,
          8,
          conf.xlen
        ),
        new IntAlu(Set(pipeline.intAlu1, pipeline.intAlu2, pipeline.intAlu3, pipeline.intAlu4)),
        new Shifter(Set(pipeline.intAlu1, pipeline.intAlu2, pipeline.intAlu3, pipeline.intAlu4)),
        new MulDiv(Set(pipeline.intMul1)),
        new BranchUnit(Set(pipeline.intAlu1, pipeline.intAlu2, pipeline.intAlu3, pipeline.intAlu4)),
        new CsrFile(pipeline.retirementStage, pipeline.intAlu1),
        new TrapHandler(pipeline.retirementStage),
        new MachineMode(pipeline.intAlu1),
        new Interrupts(pipeline.retirementStage),
        new Timers
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
    SpinalVerilog(SoC.dynamic(RamType.OnChipRam(10 MiB, args.headOption)))
  }
}

object CoreDynamicSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(SoC.dynamic(RamType.OnChipRam(10 MiB, Some(args(0))))).doSim { dut =>
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

object CoreDynamicExtMem {
  def main(args: Array[String]) {
    SpinalVerilog(SoC.dynamic(RamType.ExternalAxi4(10 MiB), 32))
  }
}

class CoreDynamicFormal extends Component {
  setDefinitionName("Core")

  implicit val config = new Config(BaseIsa.RV32I)
  val pipeline = createDynamicPipeline(extraPlugins = Seq(new RiscvFormal))
}

object CoreDynamicFormal {
  def main(args: Array[String]) {
    SpinalVerilog(new CoreDynamicFormal)
  }
}
