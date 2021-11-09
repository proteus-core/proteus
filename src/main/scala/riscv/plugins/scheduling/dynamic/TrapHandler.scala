package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

private class TrapSignals(implicit config: Config) extends Bundle {
  val hasTrapped = False
  val trapCause = UInt(4 bits).assignDontCare()
  val trapVal = UInt(config.xlen bits).assignDontCare()
}

private class StageTrapSignals(implicit config: Config) extends Area {
  val interruptSignals = new TrapSignals
  val exceptionSignals = new TrapSignals
}

class TrapHandler(trapStage: Stage)(implicit config: Config)
  extends Plugin[DynamicPipeline] with TrapService {
  private object Data {
    object HAS_TRAPPED extends PipelineData(Bool())
    object TRAP_IS_INTERRUPT extends PipelineData(Bool())
    object TRAP_CAUSE extends PipelineData(UInt(4 bits))
    object TRAP_VAL extends PipelineData(UInt(config.xlen bits))
    object MRET extends PipelineData(Bool())
  }

  private val stageSignals = mutable.Map[Stage, StageTrapSignals]()
  private val trapCommitCallbacks = mutable.ArrayBuffer[TrapCommitCallback]()

  override def setup(): Unit = {
    val issuer = pipeline.getService[IssueService]

    pipeline.getService[DecoderService].configure {decoder =>
      decoder.addDefault(Map(
        Data.MRET -> False
      ))

      decoder.addDecoding(Opcodes.MRET, InstructionType.I,
        Map(Data.MRET -> True))

      issuer.setDestination(Opcodes.MRET, pipeline.rsStages.head) // TODO: ugly?
    }

    for (stage <- pipeline.issuePipeline.stages ++ pipeline.unorderedStages :+ pipeline.retirementStage) {  // TODO?
      stageSignals(stage) = stage plug new StageTrapSignals
    }
  }

  override def build(): Unit = {
    pipeline plug {
      pipeline.issuePipeline.stages.head.input(Data.HAS_TRAPPED) := False  // TODO?
    }

    // To ensure interrupts have priority over exceptions (section 3.1.9, RISC-V
    // Privileged Architecture), we keep track of two sets of trap-related
    // signals: one for exceptions (exceptionSignals) and one for interrupts
    // (interruptSignals). The trap() function assigns to one of those based on
    // whether the trap was for an exception or interrupt. Here, we add logic
    // that checks both sets of signals and outputs interruptSignals if an
    // interrupt occurred and exceptionSignals if *only* an exception occurred.
    for ((stage, signals) <- stageSignals) {
      stage plug new Area {
        val trapSignals = new TrapSignals
        val isInterrupt = False

        when (signals.interruptSignals.hasTrapped) {
          trapSignals := signals.interruptSignals
          isInterrupt := True
        } otherwise {
          trapSignals := signals.exceptionSignals
        }

        when (trapSignals.hasTrapped) {
          stage.output(Data.HAS_TRAPPED) := True
          stage.output(Data.TRAP_IS_INTERRUPT) := isInterrupt
          stage.output(Data.TRAP_CAUSE) := trapSignals.trapCause
          stage.output(Data.TRAP_VAL) := trapSignals.trapVal
        }
      }
    }

    val jumpService = pipeline.getService[JumpService]
    val csrService = pipeline.getService[CsrService]

    val trapArea = trapStage plug new Area {
      import trapStage._

      val mstatus = slave(new CsrIo)
      val mtvec = slave(new CsrIo)
      val mcause = slave(new CsrIo)
      val mepc = slave(new CsrIo)
      val mtval = slave(new CsrIo)

      when (arbitration.isValid && value(Data.HAS_TRAPPED)) {
        val mstatusCurrent = mstatus.read()
        val mstatusNew = UInt(config.xlen bits)
        mstatusNew := mstatusCurrent
        mstatusNew(3) := False // mie = 0
        mstatusNew(7) := mstatusCurrent(3) // mpie = mie
        mstatus.write(mstatusNew)

        val cause = U(0, config.xlen bits)
        cause.msb := value(Data.TRAP_IS_INTERRUPT)
        cause(3 downto 0) := value(Data.TRAP_CAUSE)
        mcause.write(cause)

        mepc.write(value(pipeline.data.PC))
        mtval.write(value(Data.TRAP_VAL))

        val vecBase = mtvec.read()(config.xlen - 1 downto 2) << 2
        jumpService.jump(trapStage, vecBase, JumpType.Trap, checkAlignment = false)

        trapCommitCallbacks.foreach {
          _(trapStage, value(Data.TRAP_IS_INTERRUPT), value(Data.TRAP_CAUSE))
        }
      }

      when (arbitration.isValid && value(Data.MRET)) {
        val mstatusCurrent = mstatus.read()
        val mstatusNew = UInt(config.xlen bits)
        mstatusNew := mstatusCurrent
        mstatusNew(3) := mstatusCurrent(7) // mie = mpie
        mstatusNew(7) := True // mpie = 1
        mstatus.write(mstatusNew)

        jumpService.jump(trapStage, mepc.read(), JumpType.TrapReturn, checkAlignment = false)
      }
    }

    pipeline plug new Area {
      trapArea.mstatus <> csrService.getCsr(0x300)
      trapArea.mtvec <> csrService.getCsr(0x305)
      trapArea.mcause <> csrService.getCsr(0x342)
      trapArea.mepc <> csrService.getCsr(0x341)
      trapArea.mtval <> csrService.getCsr(0x343)

      // FIXME Only this part is dependent on a StaticPipeline, the rest should
      // be part of a Pipeline plugin.
      for (stage <- pipeline.issuePipeline.stages ++ pipeline.unorderedStages) {  // TODO?
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
        val ret = pipeline.retirementStage
        val laterStageTrapped = ret.arbitration.isValid && ret.output(Data.HAS_TRAPPED) // TODO: is it only the retirement stage that can trap?

        when (laterStageTrapped) {
//          stage.arbitration.isValid := False
        }
      }
    }
  }

  override def trap(stage: Stage, cause: TrapCause): Unit = {
    val stageTrapSignals = stageSignals(stage)
    val trapSignals = if (cause.isInterrupt) {
      stageTrapSignals.interruptSignals
    } else {
      stageTrapSignals.exceptionSignals
    }

    trapSignals.hasTrapped := True
    trapSignals.trapCause := U(cause.code, 4 bits)
    trapSignals.trapVal := (if (cause.mtval == null) U(0).resized else cause.mtval)
  }

  override def hasTrapped(stage: Stage): Bool = {
    stage.output(Data.HAS_TRAPPED)
  }

  override def hasException(stage: Stage): Bool = {
    hasTrapped(stage) && !stage.output(Data.TRAP_IS_INTERRUPT)
  }

  override def hasInterrupt(stage: Stage): Bool = {
    hasTrapped(stage) && stage.output(Data.TRAP_IS_INTERRUPT)
  }

  override def onTrapCommit(cb: TrapCommitCallback): Unit = {
    trapCommitCallbacks += cb
  }
}

