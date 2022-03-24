package riscv.plugins.cheri

import riscv._
import riscv.sim._
import riscv.soc._
import spinal.core._
import spinal.core.sim._

object createCheriPipeline {
  def apply(memorySize: BigInt)(implicit conf: Config): StaticPipeline = {
    val pipeline = new Component with StaticPipeline {
      setDefinitionName("Pipeline")

      val fetch = new Stage("IF")
      val decode = new Stage("ID")
      val execute = new Stage("EX")
      val memory = new Stage("MEM")
      val writeback = new Stage("WB")

      override val stages = Seq(fetch, decode, execute, memory, writeback)
      override val passThroughStage: Stage = execute // TODO: ?
      override val config: Config = conf
      override val data: StandardPipelineData = new StandardPipelineData(conf)
      override val pipelineComponent: Component = this
    }

    import riscv.{plugins => rvp}

    pipeline.addPlugins(Seq(
      new rvp.scheduling.static.Scheduler,
      new rvp.scheduling.static.DataHazardResolver(firstRsReadStage = pipeline.execute),
      new rvp.TrapHandler(pipeline.writeback),
      new rvp.TrapStageInvalidator, // TODO: ?
      new rvp.MemoryBackbone,
      new rvp.Fetcher(pipeline.fetch),
      new rvp.Decoder(pipeline.decode),
      new rvp.RegisterFileAccessor(pipeline.decode, pipeline.writeback),
      new rvp.IntAlu(Set(pipeline.execute)),
      new rvp.Shifter(Set(pipeline.execute)),
      new rvp.Lsu(Set(pipeline.memory), pipeline.memory, pipeline.memory),
      new rvp.BranchUnit(Set(pipeline.execute)),
      new rvp.scheduling.static.PcManager(0x80000000L),
      new rvp.CsrFile(pipeline.writeback, pipeline.writeback), // TODO: ugly
      new rvp.Timers,
      new rvp.MachineMode(pipeline.execute, addMepc = false, addMtvec = false),
      new rvp.Interrupts(pipeline.writeback),
      new rvp.MulDiv(Set(pipeline.execute))
    ))

    implicit val context = Context(pipeline)

    pipeline.addPlugins(Seq(
      new RegisterFile(pipeline.decode, pipeline.writeback),
      new Access(pipeline.execute),
      new ScrFile(pipeline.writeback),
      new Lsu(pipeline.memory),
      new ExceptionHandler,
      new Ccsr,
      new MemoryTagger(0x80000000L, memorySize),
      new PccManager(pipeline.execute),
      new Sealing(pipeline.execute),
      new MachineMode
    ))

    pipeline.build()
    pipeline
  }
}

object SoC {
  def static(ramType: RamType): SoC = {
    new SoC(ramType, config => createCheriPipeline(ramType.size)(config))
  }
}

object Core {
  def main(args: Array[String]) {
    SpinalVerilog(SoC.static(RamType.OnChipRam(10 MiB, args.headOption)))
  }
}

object CoreSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(SoC.static(RamType.OnChipRam(10 MiB, Some(args(0))))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      val byteDevSim = new StdioByteDev(dut.io.byteDev)

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

object CoreExtMem {
  def main(args: Array[String]) {
    SpinalVerilog(SoC.static(RamType.ExternalAxi4(10 MiB)))
  }
}
