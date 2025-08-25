package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

case class RegisterSource(indexBits: BitCount) extends Bundle {
  val priorInstructionNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorInstruction: Flow[UInt] =
    RegNext(priorInstructionNext).init(priorInstructionNext.getZero)

  def build(): Unit = {
    priorInstructionNext := priorInstruction
  }

  def reset(): Unit = {
    priorInstructionNext.setIdle()
  }
}

case class InstructionDependencies(indexBits: BitCount, speculationTracking: Boolean)
    extends Bundle {
  val rs1: RegisterSource = RegisterSource(indexBits)
  val rs2: RegisterSource = RegisterSource(indexBits)

  val priorBranchNext: Flow[UInt] = if (speculationTracking) Flow(UInt(indexBits)) else null
  val priorBranch: Flow[UInt] =
    if (speculationTracking) RegNext(priorBranchNext).init(priorBranchNext.getZero) else null

  val loadSpeculationNext: Bool = if (speculationTracking) Bool() else null
  val loadSpeculation: Bool =
    if (speculationTracking) RegNext(loadSpeculationNext).init(False) else null

  def build(): Unit = {
    rs1.build()
    rs2.build()
    if (speculationTracking) {
      priorBranchNext := priorBranch
      loadSpeculationNext := loadSpeculation
    }
  }

  def reset(): Unit = {
    rs1.reset()
    rs2.reset()
  }
}

class ReservationStation(
    exeStage: Stage,
    rob: ReorderBuffer,
    pipeline: DynamicPipeline,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area
    with CdbListener
    with Resettable {
  setPartialName(s"RS_${exeStage.stageName}")

  private val speculationTracking = pipeline.hasService[SpeculationService]

  private val meta = InstructionDependencies(rob.indexBits, speculationTracking)

  private val robEntryIndex = Reg(UInt(rob.indexBits)).init(0)

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_ARGS, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val broadcastedPsfPrediction = RegInit(False)
  private val noPsfPrediction = RegInit(False)
  private val psfPredictedAddress = Reg(UInt(config.xlen bits)).init(0)

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

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
    meta.reset()
    broadcastedPsfPrediction := False
    noPsfPrediction := False
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Prior, currentRs2Prior = Flow(UInt(rob.indexBits))
    val currentLoadSpeculation = if (speculationTracking) Bool() else null
    val branchWaiting: Flow[UInt] = if (speculationTracking) Flow(UInt(rob.indexBits)) else null

    when(state === State.EXECUTING || state === State.BROADCASTING_RESULT) {
      when(cdbMessage.robIndex === robEntryIndex) {
        cdbWaiting := False
      }
    }

    when(state === State.WAITING_FOR_ARGS) {
      currentRs1Prior := meta.rs1.priorInstruction
      currentRs2Prior := meta.rs2.priorInstruction
      if (speculationTracking) {
        branchWaiting := meta.priorBranch
        currentLoadSpeculation := meta.loadSpeculation
      }
    } otherwise {
      currentRs1Prior := meta.rs1.priorInstructionNext
      currentRs2Prior := meta.rs2.priorInstructionNext
      if (speculationTracking) {
        branchWaiting := meta.priorBranchNext
        currentLoadSpeculation := meta.loadSpeculationNext
      }
    }

    pipeline.serviceOption[SpeculationService] foreach { spec =>
      {
        // keep track of incoming branch updates, even if already executing
        when(branchWaiting.valid && cdbMessage.robIndex === branchWaiting.payload) {
          val pending = spec.speculationDependency(cdbMessage.metadata)
          when(pending.valid) {
            meta.priorBranch.push(pending.payload)
          } elsewhen (!spec.isSpeculativeCF(cdbMessage.metadata)) {
            meta.priorBranch.setIdle()
          }
        }
      }
    }

    when(state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Prior.valid
      val r2w = Bool()
      r2w := currentRs2Prior.valid
      val lsw = if (speculationTracking) Bool() else null
      if (speculationTracking) {
        lsw := currentLoadSpeculation
      }

      when(currentRs1Prior.valid && cdbMessage.robIndex === currentRs1Prior.payload) {
        meta.rs1.priorInstruction.valid := False
        r1w := False
        pipeline.serviceOption[SpeculationService] foreach { spec =>
          when(spec.isSpeculativeMD(cdbMessage.metadata)) {
            meta.loadSpeculation := True
            lsw := True
          }
        }
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when(currentRs2Prior.valid && cdbMessage.robIndex === currentRs2Prior.payload) {
        meta.rs2.priorInstruction.valid := False
        r2w := False
        pipeline.serviceOption[SpeculationService] foreach { spec =>
          when(spec.isSpeculativeMD(cdbMessage.metadata)) {
            meta.loadSpeculation := True
            lsw := True
          }
        }
        regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
      }

      when(!r1w && !r2w && !softFlush) {
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

    if (config.stlSpec) {
      // when waiting for load address, broadcast a result prediction
      when(
        state === State.WAITING_FOR_ARGS && !broadcastedPsfPrediction && !activeFlush && !noPsfPrediction
      ) {
        pipeline.serviceOption[SpeculationService] foreach { spec =>
          spec.isSpeculativeMD(cdbStream.metadata) := True
        }
        if (config.addressBasedPsf) {
          pipeline.service[LsuService].psfAddress(cdbStream.metadata) := rob.previousStoreAddress
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

      val broadcastedIncorrectPsfPrediction = Bool()

      for (register <- retirementRegisters.keys.filter(e => e != lsu.psfMisspeculationRegister)) {
        dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
      }

      pipeline.serviceOption[SpeculationService] foreach { spec =>
        {
          spec.speculationDependency(cdbStream.payload.metadata) := meta.priorBranch
          // keep control flow speculation taint if the CF speculation is resolved while still load speculating
          spec.isSpeculativeCF(cdbStream.metadata) := spec.isSpeculativeCFOutput(exeStage) || (spec
            .isSpeculativeCFInput(exeStage) && meta.loadSpeculation)
          spec.isSpeculativeMD(cdbStream.metadata) := meta.loadSpeculation
        }
      }

      // if the broadcasted address-based PSF prediction turned out to be incorrect, we have to activate the CDB again
      if (config.stlSpec && config.addressBasedPsf) {
        broadcastedIncorrectPsfPrediction := isLoad && lsu.address(
          exeStage
        ) =/= psfPredictedAddress && broadcastedPsfPrediction && !noPsfPrediction
        lsu.psfMisspeculation(cdbStream.metadata) := broadcastedIncorrectPsfPrediction
        lsu.psfMisspeculation(dispatchStream.registerMap) := broadcastedIncorrectPsfPrediction
      }

      pipeline.serviceOption[SpeculationService] match {
        case Some(spec) =>
          val condition = Bool()

          if (config.stlSpec && config.addressBasedPsf) {
            condition := exeStage.output(pipeline.data.RD_DATA_VALID) ||
              spec.isSpeculativeCFInput(exeStage) || broadcastedIncorrectPsfPrediction
          } else {
            condition := exeStage.output(pipeline.data.RD_DATA_VALID) || spec.isSpeculativeCFInput(
              exeStage
            )
          }

          when(condition) {
            cdbStream.valid := True
          }
        case None =>
          when(
            exeStage.output(pipeline.data.RD_DATA_VALID) || broadcastedIncorrectPsfPrediction
          ) {
            cdbStream.valid := True
          }
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
    }
  }

  def execute(): Unit = {
    val issueStage = pipeline.issuePipeline.stages.last

    val (robIndex, entryMeta) = rob.pushEntry()

    robEntryIndex := robIndex

    stateNext := State.EXECUTING
    regs.shift := True

    meta.reset()

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
        noPsfPrediction := True // prevent prediction on non-loads and non-word accesses
      }
    }

    if (speculationTracking) {
      val dependentJump = Flow(UInt(rob.indexBits))
      dependentJump := rob.lastSpeculativeCFInstruction
      meta.priorBranchNext := dependentJump
    }

    def dependencySetup(
        metaRs: RegisterSource,
        rsData: Flow[RsData],
        regData: PipelineData[UInt]
    ): Unit = {
      when(rsData.valid) {
        when(rsData.payload.updatingInstructionFound) {
          when(rsData.payload.updatingInstructionFinished) {
            regs.setReg(regData, rsData.payload.updatingInstructionValue)
          } otherwise {
            metaRs.priorInstructionNext.push(rsData.payload.updatingInstructionIndex)
          }
        } otherwise {
          regs.setReg(regData, issueStage.output(regData))
        }
        when(
          rsData.payload.updatingInstructionFound && !rsData.payload.updatingInstructionFinished
        ) {
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    dependencySetup(meta.rs1, entryMeta.rs1Data, pipeline.data.RS1_DATA)
    dependencySetup(meta.rs2, entryMeta.rs2Data, pipeline.data.RS2_DATA)

    if (speculationTracking) {
      meta.loadSpeculationNext := entryMeta.rs1Data.updatingInstructionLoadSpeculation || entryMeta.rs2Data.updatingInstructionLoadSpeculation
    }
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
