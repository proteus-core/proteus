package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.{Flow, Stream}

case class RegisterSource(indexBits: BitCount) extends Bundle {
  val priorInstructionNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorInstruction: Flow[UInt] =
    RegNext(priorInstructionNext).init(priorInstructionNext.getZero)

  val isSecretNext: Bool = Bool()
  val isSecret: Bool = RegNext(isSecretNext).init(False)

  def build(): Unit = {
    priorInstructionNext := priorInstruction
    isSecretNext := isSecret
  }

  def reset(): Unit = {
    priorInstructionNext.setIdle()
    isSecretNext := False
  }
}

case class InstructionDependencies(indexBits: BitCount) extends Bundle {
  val rs1: RegisterSource = RegisterSource(indexBits)
  val rs2: RegisterSource = RegisterSource(indexBits)

  val priorBranchNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorBranch: Flow[UInt] = RegNext(priorBranchNext).init(priorBranchNext.getZero)

  def build(): Unit = {
    rs1.build()
    rs2.build()
    priorBranchNext := priorBranch
  }

  def reset(): Unit = {
    rs1.reset()
    rs2.reset()
    priorBranchNext.setIdle()
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

  private val meta = InstructionDependencies(rob.indexBits)

  private val robEntryIndex = Reg(UInt(rob.indexBits)).init(0)

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_ARGS, EXECUTING, BROADCASTING_RESULT = newElement()
  }

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

  val prospectResolved: Bool = Bool()
  prospectResolved := False

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
    meta.reset()
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Prior, currentRs2Prior = Flow(UInt(rob.indexBits))
    val branchWaiting = Flow(UInt(rob.indexBits))
    val rs1secret = Bool()
    val rs2secret = Bool()
    if (!pipeline.hasService[ProspectService]) {
      branchWaiting.setIdle()
      rs1secret := False
      rs2secret := False
    }

    when(state === State.WAITING_FOR_ARGS) {
      currentRs1Prior := meta.rs1.priorInstruction
      currentRs2Prior := meta.rs2.priorInstruction
      if (pipeline.hasService[ProspectService]) {
        branchWaiting := meta.priorBranch
        rs1secret := meta.rs1.isSecret
        rs2secret := meta.rs2.isSecret
      }
    } otherwise {
      currentRs1Prior := meta.rs1.priorInstructionNext
      currentRs2Prior := meta.rs2.priorInstructionNext
      if (pipeline.hasService[ProspectService]) {
        branchWaiting := meta.priorBranchNext
        rs1secret := meta.rs1.isSecretNext
        rs2secret := meta.rs2.isSecretNext
      }
    }

    if (pipeline.hasService[ProspectService]) {
      // keep track of incoming branch updates, even if already executing
      when(branchWaiting.valid && cdbMessage.robIndex === branchWaiting.payload) {
        val pending = pipeline.service[BranchService].pendingBranchOfBundle(cdbMessage.metadata)
        val targetService = pipeline.service[BranchTargetPredictorService]
        when(pending.valid) {
          meta.priorBranch.push(pending.payload.resized) // TODO: resized
        } elsewhen (targetService
          .predictedPcOfBundle(cdbMessage.metadata) === cdbMessage.metadata.elementAs[UInt](
          pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
        )) { // when the branch was correctly predicted
          meta.priorBranch.setIdle()
          when(!currentRs1Prior.valid && !currentRs2Prior.valid) {
            when(state === State.WAITING_FOR_ARGS) {
              state := State.EXECUTING
            }
            prospectResolved := True
          }
        } // otherwise: wait for pipeline reset
      }
    }

    when(state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Prior.valid
      val r2w = Bool()
      r2w := currentRs2Prior.valid
      val brw = Bool()
      brw := branchWaiting.valid
      val sec1 = Bool()
      sec1 := rs1secret
      val sec2 = Bool()
      sec2 := rs1secret

      when(currentRs1Prior.valid && cdbMessage.robIndex === currentRs1Prior.payload) {
        pipeline.serviceOption[ProspectService] foreach { prospect =>
          meta.rs1.isSecret := prospect.isSecretOfBundle(cdbMessage.metadata)
          sec1 := prospect.isSecretOfBundle(cdbMessage.metadata)
        }
        meta.rs1.priorInstruction.valid := False
        r1w := False
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when(currentRs2Prior.valid && cdbMessage.robIndex === currentRs2Prior.payload) {
        pipeline.serviceOption[ProspectService] foreach { prospect =>
          meta.rs2.isSecret := prospect.isSecretOfBundle(cdbMessage.metadata)
          sec2 := prospect.isSecretOfBundle(cdbMessage.metadata)
        }
        meta.rs2.priorInstruction.valid := False
        r2w := False
        regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
      }

      // ProSpeCT: keep stalling if one of the operands is secret and there is a pending branch
      val transientSecretAccess = Bool()
      if (pipeline.hasService[ProspectService]) {
        transientSecretAccess := brw && (sec1 || sec2)
      } else {
        transientSecretAccess := False
      }

      when(!r1w && !r2w && !transientSecretAccess) {
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

    // execution was invalidated while running
    when(activeFlush) {
      reset()
    }

    // when waiting for the result, and it is ready, put in on the bus
    when(state === State.EXECUTING && exeStage.arbitration.isDone && !activeFlush) {
      cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)
      cdbStream.metadata.elementAs[UInt](
        pipeline.data.NEXT_PC.asInstanceOf[PipelineData[Data]]
      ) := exeStage.output(pipeline.data.NEXT_PC)

      val outputIsSecret = Bool()

      pipeline.serviceOption[ProspectService] match {
        case Some(prospect) => {
          // ProSpeCT: the output is tainted if one of the inputs is tainted or the execution stage tainted the result
          outputIsSecret := meta.rs1.isSecret || meta.rs2.isSecret || prospect.isSecret(exeStage)
          // propagate branch dependencies on the CDB
          pipeline
            .service[BranchService]
            .pendingBranchOfBundle(cdbStream.payload.metadata) := meta.priorBranch.resized
          val targetService = pipeline.service[BranchTargetPredictorService]
          targetService.predictedPcOfBundle(cdbStream.metadata) := targetService.predictedPc(
            exeStage
          )
        }
        case None => outputIsSecret := False
      }

      pipeline.serviceOption[ProspectService] foreach { prospect =>
        prospect.isSecretOfBundle(cdbStream.payload.metadata) := outputIsSecret
      }

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex

      for (register <- retirementRegisters.keys) {
        pipeline.serviceOption[ProspectService] match {
          case Some(prospect) => {
            if (prospect.isSecretPipelineReg(register)) {
              dispatchStream.payload.registerMap.element(register) := outputIsSecret
            } else {
              dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
            }
          }
          case None =>
            dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
        }
      }

      when(
        exeStage
          .output(pipeline.data.RD_DATA_VALID) || pipeline.service[BranchService].isBranch(exeStage)
      ) {
        cdbStream.valid := True
      }
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
  }

  def execute(): Unit = {
    val issueStage = pipeline.issuePipeline.stages.last

    val robIndex = UInt()
    robIndex := rob.pushEntry(
      issueStage.output(pipeline.data.RD),
      issueStage.output(pipeline.data.RD_TYPE),
      pipeline.service[LsuService].operationOutput(issueStage),
      pipeline.service[BranchService].isBranch(issueStage),
      issueStage.output(pipeline.data.PC),
      pipeline.service[BranchTargetPredictorService].predictedPc(issueStage)
    )

    robEntryIndex := robIndex

    stateNext := State.EXECUTING
    regs.shift := True

    meta.reset()

    val dependentJump = Flow(UInt(rob.indexBits))
    if (pipeline.hasService[ProspectService]) {
      dependentJump := rob.hasUnresolvedBranch
      when(dependentJump.valid) {
        meta.priorBranchNext.push(dependentJump.payload)
      }
    } else {
      dependentJump.setIdle()
    }

    def dependencySetup(
        metaRs: RegisterSource,
        reg: PipelineData[UInt],
        regData: PipelineData[UInt],
        regType: PipelineData[SpinalEnumCraft[RegisterType.type]]
    ): Unit = {
      val rsUsed = issueStage.output(regType) === RegisterType.GPR

      when(rsUsed) {
        val rsReg = issueStage.output(reg)
        val (rsInRob, rsValue) = rob.findRegisterValue(rsReg)

        when(rsInRob) {
          when(rsValue.valid) {
            pipeline.serviceOption[ProspectService] foreach { prospect =>
              metaRs.isSecretNext := prospect.isSecretOfBundle(rsValue.payload.metadata)
            }
            regs.setReg(regData, rsValue.payload.writeValue)
          } otherwise {
            metaRs.priorInstructionNext.push(rsValue.payload.robIndex)
          }
        } otherwise {
          pipeline.serviceOption[ProspectService] foreach { prospect =>
            metaRs.isSecretNext := prospect.isSecretRegister(rsReg)
          }
          regs.setReg(regData, issueStage.output(regData))
        }

        // ProSpeCT: condition for waiting: either the operand is pending in ROB, or the operand is secret and there is a pending branch
        when(
          (rsInRob && !rsValue.valid) || (dependentJump.valid && metaRs.isSecretNext && !prospectResolved)
        ) {
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    dependencySetup(meta.rs1, pipeline.data.RS1, pipeline.data.RS1_DATA, pipeline.data.RS1_TYPE)
    dependencySetup(meta.rs2, pipeline.data.RS2, pipeline.data.RS2_DATA, pipeline.data.RS2_TYPE)
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
