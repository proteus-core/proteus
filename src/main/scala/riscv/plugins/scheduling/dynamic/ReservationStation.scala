package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

case class RegisterSource(indexBits: BitCount) extends Bundle {
  val priorInstructionNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorInstruction: Flow[UInt] =
    RegNext(priorInstructionNext).init(priorInstructionNext.getZero)

  val taintedNext: Bool = Bool()
  val tainted: Bool = RegNext(taintedNext).init(False)

  def build(): Unit = {
    priorInstructionNext := priorInstruction
    taintedNext := tainted
  }

  def reset(): Unit = {
    priorInstructionNext.setIdle()
    taintedNext := False
  }
}

case class InstructionDependencies(
    indexBits: BitCount,
    cfSpeculationTracking: Boolean,
    dataSpeculationTracking: Boolean,
    rfence: Boolean
) extends Bundle {
  val rs1: RegisterSource = RegisterSource(indexBits)
  val rs2: RegisterSource = RegisterSource(indexBits)

  val priorBranchNext: Flow[UInt] = if (cfSpeculationTracking) Flow(UInt(indexBits)) else null
  val priorBranch: Flow[UInt] =
    if (cfSpeculationTracking) RegNext(priorBranchNext).init(priorBranchNext.getZero) else null

  val loadSpeculationNext: Bool = if (dataSpeculationTracking) Bool() else null
  val loadSpeculation: Bool =
    if (dataSpeculationTracking) RegNext(loadSpeculationNext).init(False) else null

  val simplifiedRegisterFence = rfence && !cfSpeculationTracking
  val optimizedRegisterFence = rfence && cfSpeculationTracking

  val registerFenceNext = if (rfence) Bool() else null
  val registerFence =
    if (rfence) RegNext(registerFenceNext).init(False)
    else null

  def build(): Unit = {
    rs1.build()
    rs2.build()
    if (cfSpeculationTracking) {
      priorBranchNext := priorBranch
    }
    if (dataSpeculationTracking) {
      loadSpeculationNext := loadSpeculation
    }
    if (rfence) {
      registerFenceNext := registerFence
    }
  }

  def reset(): Unit = {
    rs1.reset()
    rs2.reset()
    if (rfence) {
      registerFenceNext := False
    }
  }
}

class ReservationStation(
    exeStage: Stage,
    rob: ReorderBuffer,
    pipeline: DynamicPipeline,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: DynamicPipelineConfig)
    extends Area
    with CdbListener
    with Resettable {
  setPartialName(s"RS_${exeStage.stageName}")

  private val cfSpeculationTracking = pipeline.hasService[ControlSpeculationService]
  private val dataSpeculationTracking = pipeline.hasService[DataSpeculationService]

  private val meta = InstructionDependencies(
    rob.indexBits,
    cfSpeculationTracking,
    dataSpeculationTracking,
    pipeline.hasService[RegisterFenceService]
  )

  private val robEntryIndex = Reg(UInt(rob.indexBits)).init(0)

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_ARGS, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val broadcastedPsfPrediction = RegInit(False)
  private val noPsfPrediction = RegInit(False)
  private val psfPredictedAddress = Reg(UInt(config.isa.xlen bits)).init(0)

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)

  private val cdbWaitingNext, dispatchWaitingNext = Bool()
  private val cdbWaiting = RegNext(cdbWaitingNext).init(False)
  private val dispatchWaiting = RegNext(dispatchWaitingNext).init(False)

  private val resultCdbMessage = RegInit(CdbMessage(metaRegisters, rob.indexBits).getZero)
  private val resultDispatchMessage = RegInit(
    RdbMessage(retirementRegisters, rob.indexBits).getZero
  )

  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(metaRegisters, rob.indexBits)))
  val dispatchStream: Stream[RdbMessage] = Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))
  )

  private val regs = pipeline.pipelineRegs(exeStage) // TODO: do we need this?

  val isAvailable: Bool = Bool()

  val activeFlush: Bool = Bool()
  val softFlush: Bool = Bool()

  val speculationResolved: Bool = Bool()
  speculationResolved := False

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
    meta.reset()
    broadcastedPsfPrediction := False
    noPsfPrediction := False
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Prior, currentRs2Prior = Flow(UInt(rob.indexBits))
    val currentRegisterFence = Bool()
    val currentLoadSpeculation = if (dataSpeculationTracking) Bool() else null
    val branchWaiting: Flow[UInt] = if (cfSpeculationTracking) Flow(UInt(rob.indexBits)) else null

    val rs1Tainted = Bool()
    val rs2Tainted = Bool()
    if (!pipeline.hasService[PipelineTaintService]) {
      rs1Tainted := False
      rs2Tainted := False
    }

    when(state === State.EXECUTING || state === State.BROADCASTING_RESULT) {
      when(cdbMessage.robIndex === robEntryIndex) {
        cdbWaiting := False
      }
    }

    when(state === State.WAITING_FOR_ARGS) {
      currentRs1Prior := meta.rs1.priorInstruction
      currentRs2Prior := meta.rs2.priorInstruction
      if (cfSpeculationTracking) {
        branchWaiting := meta.priorBranch
      }
      if (dataSpeculationTracking) {
        currentLoadSpeculation := meta.loadSpeculation
      }
      if (meta.rfence) {
        currentRegisterFence := meta.registerFence
      } else {
        currentRegisterFence := False
      }
      if (pipeline.hasService[PipelineTaintService]) {
        rs1Tainted := meta.rs1.tainted
        rs2Tainted := meta.rs2.tainted
      }
    } otherwise {
      currentRs1Prior := meta.rs1.priorInstructionNext
      currentRs2Prior := meta.rs2.priorInstructionNext
      if (cfSpeculationTracking) {
        branchWaiting := meta.priorBranchNext
      }
      if (dataSpeculationTracking) {
        currentLoadSpeculation := meta.loadSpeculationNext
      }
      if (meta.rfence) {
        currentRegisterFence := meta.registerFenceNext
      } else {
        currentRegisterFence := False
      }
      if (pipeline.hasService[PipelineTaintService]) {
        rs1Tainted := meta.rs1.taintedNext
        rs2Tainted := meta.rs2.taintedNext
      }
    }

    pipeline.serviceOption[ControlSpeculationService] foreach { spec =>
      // keep track of incoming branch updates, even if already executing
      when(branchWaiting.valid && cdbMessage.robIndex === branchWaiting.payload) {
        val pending = spec.speculationDependency(cdbMessage.metadata)
        when(pending.valid) {
          meta.priorBranch.push(pending.payload)
        } elsewhen (!spec.isSpeculativeCF(cdbMessage.metadata)) {
          meta.priorBranch.setIdle()
          if (meta.optimizedRegisterFence) {
            when(!currentRs1Prior.valid && !currentRs2Prior.valid) {
              when(state === State.WAITING_FOR_ARGS) {
                state := State.EXECUTING
              }
              speculationResolved := True
            }
          }
        }
      }
    }

    when(state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Prior.valid
      val r2w = Bool()
      r2w := currentRs2Prior.valid
      val lsw = if (dataSpeculationTracking) Bool() else null
      if (dataSpeculationTracking) {
        lsw := currentLoadSpeculation
      }
      val tnt1 = Bool()
      tnt1 := rs1Tainted
      val tnt2 = Bool()
      tnt2 := rs2Tainted

      when(currentRs1Prior.valid && cdbMessage.robIndex === currentRs1Prior.payload) {
        meta.rs1.priorInstruction.valid := False
        when(currentRegisterFence) {
          pipeline.serviceOption[DataSpeculationService] foreach { spec =>
            // only start executing fence if the update was non-speculative
            when(spec.isSsbSpeculative(cdbMessage.metadata)) {
              r1w := False
            }
          }
        } otherwise {
          r1w := False
        }
        pipeline.serviceOption[DataSpeculationService] foreach { spec =>
          when(spec.isSsbSpeculative(cdbMessage.metadata)) {
            meta.loadSpeculation := True
            lsw := True
          }
        }
        pipeline.serviceOption[PipelineTaintService] foreach { tracking =>
          tnt1 := tracking.tainted(cdbMessage.metadata)
          meta.rs1.tainted := tracking.tainted(cdbMessage.metadata)
        }
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when(currentRs2Prior.valid && cdbMessage.robIndex === currentRs2Prior.payload) {
        meta.rs2.priorInstruction.valid := False
        r2w := False
        pipeline.serviceOption[DataSpeculationService] foreach { spec =>
          when(spec.isSsbSpeculative(cdbMessage.metadata)) {
            meta.loadSpeculation := True
            lsw := True
          }
        }
        pipeline.serviceOption[PipelineTaintService] foreach { tracking =>
          tnt2 := tracking.tainted(cdbMessage.metadata)
          meta.rs2.tainted := tracking.tainted(cdbMessage.metadata)
        }
        regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
      }

      val baseCondition = !r1w && !r2w && !softFlush
      val startExecution = Bool()

      val registerFenceWaiting = Bool()
      if (meta.optimizedRegisterFence) {
        registerFenceWaiting := (branchWaiting.valid || lsw) && currentRegisterFence
      } else if (meta.simplifiedRegisterFence) {
        registerFenceWaiting := currentRegisterFence
      } else {
        registerFenceWaiting := False
      }

      if (pipeline.hasService[ProSpeCTService]) {
        startExecution := baseCondition && !(branchWaiting.valid && (tnt1 || tnt2)) // TODO: what about load speculation?
      } else if (pipeline.hasService[RegisterFenceService]) {
        startExecution := baseCondition && !registerFenceWaiting
      } else {
        startExecution := baseCondition
      }

      when(startExecution) {
        // This is the only place where state is written directly (instead of
        // via stateNext). This ensures that we have priority over whatever
        // execute() writes to it which means that the order of calling
        // execute() and processUpdate() doesn't matter. We need this priority
        // to ensure we start executing when execute() is called in the same
        // cycle as (one of) its arguments arrive(s) via processUpdate().
        state := State.EXECUTING
      }
    }
  }

  def build(): Unit = {
    meta.build()

    cdbWaitingNext := cdbWaiting
    dispatchWaitingNext := dispatchWaiting

    stateNext := state

    dispatchStream.valid := False
    dispatchStream.payload := resultDispatchMessage

    cdbStream.valid := False
    cdbStream.payload := resultCdbMessage

    pipeline.serviceOption[ControlSpeculationService] foreach { spec =>
      when(spec.speculationDependency(resultCdbMessage.metadata) =/= meta.priorBranch) {
        spec.speculationDependency(cdbStream.payload.metadata) := meta.priorBranch
      }
    }

    regs.shift := False

    exeStage.arbitration.isStalled := state === State.WAITING_FOR_ARGS
    exeStage.arbitration.isValid :=
      (state === State.WAITING_FOR_ARGS) || (state === State.EXECUTING)

    isAvailable := False

    when(state === State.IDLE) {
      isAvailable := !activeFlush
    }

    activeFlush := False
    softFlush := False

    // execution was invalidated while running
    when(activeFlush) {
      reset()
    }

    // for any fence, execution can continue when it's the last entry in the ROB
    if (meta.rfence) {
      when(
        state === State.WAITING_FOR_ARGS && meta.registerFence && robEntryIndex === rob.oldestIndex
      ) {
        meta.registerFenceNext := False
        state := State.EXECUTING
      }
    }

    /** This is predictive store forwarding (PSF): for appropriate load instructions (indicated by a
      * False broadcastedPsfPrediction) we broadcast the value of the last store instruction if the
      * current load address is not yet available.
      *
      * This means that loads with a PSF prediction will issue two CDB messages: this first one is
      * the prediction, then the LoadManager will broadcast the actual loaded value once the address
      * is resolved and the load executed.
      */
    if (config.stlSpec) {
      when(
        state === State.WAITING_FOR_ARGS && !broadcastedPsfPrediction && !activeFlush && !noPsfPrediction
      ) {
        pipeline.serviceOption[DataSpeculationService] foreach { spec =>
          spec.isSsbSpeculative(cdbStream.metadata) := True
        }
        if (config.addressBasedPsf) {
          psfPredictedAddress := rob.previousStoreAddress
        }
        cdbStream.payload.writeValue := rob.previousStoreBuffer
        cdbStream.payload.robIndex := robEntryIndex
        cdbStream.valid := True
        when(cdbStream.ready) {
          broadcastedPsfPrediction := True
        }
      }
    }

    // when waiting for the result, and it is ready, put in on the bus
    when(state === State.EXECUTING && exeStage.arbitration.isDone && !activeFlush) {
      cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex

      val lsu = pipeline.service[LsuService]

      val isLoad = lsu.operationOutput(exeStage) === LsuOperationType.LOAD

      for (register <- retirementRegisters.keys) {
        dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
      }

      pipeline.serviceOption[DataSpeculationService] foreach { spec =>
        spec.isSsbSpeculative(cdbStream.metadata) := meta.loadSpeculation
      }

      pipeline.serviceOption[ControlSpeculationService] foreach { spec =>
        // keep control flow speculation taint if the CF speculation is resolved while still load speculating
        spec.isSpeculativeCF(cdbStream.metadata) := spec.isSpeculativeCFOutput(exeStage) || (spec
          .isSpeculativeCFInput(exeStage) && meta.loadSpeculation)
      }

      pipeline.serviceOption[PipelineTaintService] foreach { tracking =>
        val outputTainted = meta.rs1.tainted || meta.rs2.tainted || tracking.tainted(exeStage)

        tracking.tainted(cdbStream.payload.metadata) := outputTainted
        when(outputTainted) {
          tracking.tainted(dispatchStream.payload.registerMap) := True
        }
      }

      /** When the correctness of the PSF prediction is determined based on the address of the load
        * and the store matching, we can already determine here whether the prediction was
        * incorrect.
        */
      val broadcastedIncorrectPsfPrediction: Bool = if (config.stlSpec) Bool() else null
      if (config.stlSpec && config.addressBasedPsf) {
        broadcastedIncorrectPsfPrediction := isLoad &&
          lsu.address(exeStage) =/= psfPredictedAddress &&
          broadcastedPsfPrediction && !noPsfPrediction
        pipeline.serviceOption[DataSpeculationService] foreach { spec =>
          spec.isPsfSpeculative(cdbStream.metadata) := broadcastedIncorrectPsfPrediction
          // conditional to avoid assignment overlap
          when(broadcastedIncorrectPsfPrediction) {
            spec.isPsfSpeculative(dispatchStream.registerMap) := True
          }
        }
      }

      /** we want to send a CDB message when it has an output value or when speculation tracking
        * demands it
        */

      val cdbStreamActivate = Bool()
      cdbStreamActivate := exeStage.output(pipeline.data.RD_DATA_VALID)

      if (config.stlSpec && config.addressBasedPsf) {
        when(broadcastedIncorrectPsfPrediction) {
          cdbStreamActivate := True
        }
      }

      pipeline.serviceOption[ControlSpeculationService] foreach { spec =>
        when(spec.isSpeculativeCFInput(exeStage)) {
          cdbStreamActivate := True
        }
      }

      when(cdbStreamActivate) {
        cdbStream.valid := True
      }

      dispatchStream.payload.willCdbUpdate := cdbStream.valid || isLoad
      dispatchStream.valid := True

      // Override the assignment of resultCdbMessage to make sure data can be sent in later cycles
      // in case of contention in this cycle
      resultCdbMessage := cdbStream.payload
      resultDispatchMessage := dispatchStream.payload

      cdbWaitingNext := (!cdbStream.ready && cdbStream.valid)
      dispatchWaitingNext := !dispatchStream.ready

      when((cdbStream.ready || !cdbStream.valid) && dispatchStream.ready) {
        reset()
      } otherwise {
        stateNext := State.BROADCASTING_RESULT
      }
    }

    // if the result is on the buses and it has been acknowledged, make the RS
    // available again
    when(state === State.BROADCASTING_RESULT && !activeFlush) {
      cdbStream.valid := cdbWaiting
      dispatchStream.valid := dispatchWaiting

      when(cdbStream.ready && cdbWaiting) {
        cdbWaitingNext := False
      }

      when(dispatchStream.ready && dispatchWaiting) {
        dispatchWaitingNext := False
      }

      when((cdbStream.ready || !cdbWaiting) && (dispatchStream.ready || !dispatchWaiting)) {
        reset()
      }
    }

    when(rob.softResetThisCycle && state =/= State.IDLE) {
      when(
        rob.relativeIndexForAbsolute(robEntryIndex) > rob.relativeIndexForAbsolute(
          rob.currentSoftResetTrigger.payload
        )
      ) {
        reset()
        softFlush := True
      }
    } otherwise {
      // fail-safe in case of overtainting: start execution when there is nothing to wait for
      when(robEntryIndex === rob.oldestIndex && state === State.WAITING_FOR_ARGS) {
        stateNext := State.EXECUTING
      }
    }
  }

  def execute(): Unit = {
    val issueStage = pipeline.issuePipeline.stages.last

    val (robIndex, entryMeta) = rob.pushEntry()

    robEntryIndex := robIndex

    stateNext := State.EXECUTING
    regs.shift := True

    meta.reset()

    pipeline.serviceOption[RegisterFenceService].foreach { rf =>
      when(rf.isRegisterFence(issueStage) && robEntryIndex =/= rob.oldestIndex) {
        meta.registerFenceNext := True
        if (meta.simplifiedRegisterFence) {
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    /** We only want to perform PSF on word-width load instructions (for now)
      */
    if (config.stlSpec) {
      broadcastedPsfPrediction := False
      when(
        pipeline
          .service[LsuService]
          .operationOutput(issueStage) =/= LsuOperationType.LOAD || pipeline
          .service[LsuService]
          .widthOut(issueStage) =/= LsuAccessWidth.W ||
          entryMeta.preventPsf
      ) {
        noPsfPrediction := True
      }
    }

    val dependentJump =
      if (cfSpeculationTracking) Flow(UInt(rob.indexBits)) else null
    if (cfSpeculationTracking) {
      dependentJump := rob.lastSpeculativeCFInstruction
      meta.priorBranchNext := dependentJump
    }

    def dependencySetup(
        metaRs: RegisterSource,
        rsData: Flow[RsData],
        regData: PipelineData[UInt],
        regId: PipelineData[UInt]
    ): Unit = {
      when(rsData.valid) {
        when(rsData.payload.updatingInstructionFound) {
          when(rsData.payload.updatingInstructionFinished) {
            regs.setReg(regData, rsData.payload.updatingInstructionValue)
            pipeline.serviceOption[PipelineTaintService] foreach { tracking =>
              metaRs.taintedNext := rsData.tainted
            }
          } otherwise {
            metaRs.priorInstructionNext.push(rsData.payload.updatingInstructionIndex)
          }
        } otherwise {
          regs.setReg(regData, issueStage.output(regData))
          pipeline.serviceOption[PipelineTaintService] foreach { tracking =>
            metaRs.taintedNext := tracking.registerTaint(issueStage.output(regId))
          }
        }

        if (meta.optimizedRegisterFence) {
          when(
            (rsData.payload.updatingInstructionFound && !rsData.payload.updatingInstructionFinished) ||
              (dependentJump.valid && meta.registerFenceNext && !speculationResolved)
          ) {
            stateNext := State.WAITING_FOR_ARGS
          }
        } else {
          when(
            rsData.payload.updatingInstructionFound && !rsData.payload.updatingInstructionFinished
          ) {
            stateNext := State.WAITING_FOR_ARGS
          }
        }
      }
    }

    dependencySetup(meta.rs1, entryMeta.rs1Data, pipeline.data.RS1_DATA, pipeline.data.RS1)
    dependencySetup(meta.rs2, entryMeta.rs2Data, pipeline.data.RS2_DATA, pipeline.data.RS2)

    if (dataSpeculationTracking) {
      meta.loadSpeculationNext := entryMeta.rs1Data.updatingInstructionLoadSpeculation || entryMeta.rs2Data.updatingInstructionLoadSpeculation
      // TODO: maybe the following is useless (would need to specifically identify PSF predictions, but it's tricky, would need to track
      // them separately)
      if (pipeline.hasService[RegisterFenceService]) {
        when(
          meta.registerFenceNext && entryMeta.rs1Data.updatingInstructionLoadSpeculation && !entryMeta.rs1Data.updatingInstructionFinished
        ) {
          meta.rs1.priorInstructionNext.push(entryMeta.rs1Data.payload.updatingInstructionIndex)
        }
      }
    }
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
