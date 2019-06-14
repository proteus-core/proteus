package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class TrapHandler(implicit config: Config) extends Plugin with TrapService {
  private object Data {
    object HAS_TRAPPED extends PipelineData(Bool())
    object TRAP_IS_INTERRUPT extends PipelineData(Bool())
    object TRAP_CAUSE extends PipelineData(UInt(4 bits))
    object TRAP_VAL extends PipelineData(UInt(config.xlen bits))
  }

  override def setup(pipeline: Pipeline): Unit = {
    pipeline.getService[DecoderService].configure(pipeline) {decoder =>
      decoder.addDefault(Data.HAS_TRAPPED, False)
    }
  }

  override def build(pipeline: Pipeline): Unit = {
    val jumpService = pipeline.getService[JumpService]
    val csrService = pipeline.getService[CsrService]

    val trapStage = pipeline.stages.last

    val trapArea = trapStage plug new Area {
      import trapStage._

      val mtvec = slave(new CsrIo)
      val mcause = slave(new CsrIo)
      val mepc = slave(new CsrIo)
      val mtval = slave(new CsrIo)

      when (arbitration.isValid && value(Data.HAS_TRAPPED)) {
        val cause = U(0, config.xlen bits)
        cause.msb := value(Data.TRAP_IS_INTERRUPT)
        cause(3 downto 0) := value(Data.TRAP_CAUSE)
        mcause.write(cause)

        mepc.write(value(pipeline.data.PC))
        mtval.write(value(Data.TRAP_VAL))

        val vecBase = mtvec.read()(config.xlen - 1 downto 2) << 2
        jumpService.jump(pipeline, trapStage, vecBase, isTrap = true)
      }
    }

    pipeline plug new Area {
      trapArea.mtvec <> csrService.getCsr(pipeline, 0x305)
      trapArea.mcause <> csrService.getCsr(pipeline, 0x342)
      trapArea.mepc <> csrService.getCsr(pipeline, 0x341)
      trapArea.mtval <> csrService.getCsr(pipeline, 0x343)

      for (stage <- pipeline.stages.init) {
        // Make isValid False whenever there is a later stage that has a trapped
        // instruction. This basically ensures the whole pipeline behind a
        // trapped instruction is flushed until the trapped instruction is
        // committed.
        // FIXME This setup could still go wrong in the following scenario: if
        // there would be a stage that commits something to an architectural
        // state and is more than one stage before a stage that could possible
        // trap, then these architectural state changes could become visible
        // even though the instruction never reaches WB. In the current static
        // 5-stage pipeline this can never happen because the earliest stage
        // that changes architectural state is MEM which is only one stage
        // before WB (which can trap).
        val laterStages = pipeline.stages.dropWhile(_ != stage).tail
        val laterStageTrapped = laterStages.map(s => {
          s.arbitration.isValid && s.output(Data.HAS_TRAPPED)
        }).orR

        when (laterStageTrapped) {
          stage.arbitration.isValid := False
        }
      }
    }
  }

  override def trap(pipeline: Pipeline, stage: Stage, cause: TrapCause): Unit = {
    stage.output(Data.HAS_TRAPPED) := True
    stage.output(Data.TRAP_IS_INTERRUPT) := Bool(cause.isInterrupt)
    stage.output(Data.TRAP_CAUSE) := U(cause.code, 4 bits)

    val mtval = if (cause.mtval == null) U(0).resized else cause.mtval
    stage.output(Data.TRAP_VAL) := mtval
  }

  override def hasTrapped(pipeline: Pipeline, stage: Stage): Bool = {
    stage.output(Data.HAS_TRAPPED)
  }
}
