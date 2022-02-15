package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.{Flow, Stream}

trait Resettable {
  def pipelineReset(): Unit
}

// TODO: think about these names
case class RegisterSource(indexBits: BitCount) extends Bundle {
  val priorInstructionNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorInstruction: Flow[UInt] = RegNext(priorInstructionNext).init(priorInstructionNext.getZero)
  val priorBranchNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorBranch: Flow[UInt] = RegNext(priorBranchNext).init(priorBranchNext.getZero)

  def waiting: Bool = priorInstruction.valid
  def waitingNext: Bool = priorInstructionNext.valid

  def build(): Unit = {
    priorInstructionNext := priorInstruction
    priorBranchNext := priorBranch
  }
}

class ReservationStation(exeStage: Stage,
                         rob: ReorderBuffer,
                         pipeline: DynamicPipeline,
                         retirementRegisters: DynBundle[PipelineData[Data]],
                         metaRegisters: DynBundle[PipelineData[Data]])
                        (implicit config: Config) extends Area with CdbListener with Resettable {
  setPartialName(s"RS_${exeStage.stageName}")

  private val rs1Source = RegisterSource(rob.indexBits)
  private val rs2Source = RegisterSource(rob.indexBits)

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
  private val resultDispatchMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)

  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(metaRegisters, rob.indexBits)))
  val dispatchStream: Stream[RdbMessage] = Stream(HardType(RdbMessage(retirementRegisters, rob.indexBits)))

  private val regs = pipeline.pipelineRegs(exeStage) // TODO: do we need this?

  val isAvailable: Bool = Bool()

  val activeFlush: Bool = Bool()

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Waiting, currentRs2Waiting = Bool()
    val currentRs1RobIndex, currentRs2RobIndex = UInt(rob.indexBits)

    when (state === State.WAITING_FOR_ARGS) {
      currentRs1Waiting := rs1Source.waiting
      currentRs2Waiting := rs2Source.waiting
      currentRs1RobIndex := rs1Source.priorInstruction.payload
      currentRs2RobIndex := rs2Source.priorInstruction.payload
    } otherwise {
      currentRs1Waiting := rs1Source.waitingNext
      currentRs2Waiting := rs2Source.waitingNext
      currentRs1RobIndex := rs1Source.priorInstructionNext.payload
      currentRs2RobIndex := rs2Source.priorInstructionNext.payload
    }

    when (state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Waiting
      val r2w = Bool()
      r2w := currentRs2Waiting

      when (currentRs1Waiting && cdbMessage.robIndex === currentRs1RobIndex) {
        pipeline.withService[ContextService](
          context => {
            when(!context.isTransientSecretOfBundle(cdbMessage.metadata)) {
              rs1Source.priorInstruction.valid := False
              r1w := False
            }
          },
          {
            rs1Source.priorInstruction.valid := False
            r1w := False
          }
        )
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when (currentRs2Waiting && cdbMessage.robIndex === currentRs2RobIndex) {
        pipeline.withService[ContextService](
          context => {
            when(!context.isTransientSecretOfBundle(cdbMessage.metadata)) {
              rs2Source.priorInstruction.valid := False
              r2w := False
            }
          },
          {
            rs2Source.priorInstruction.valid := False
            r2w := False
          }
        )
        regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
      }

      when (!r1w && !r2w) {
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
    rs1Source.build()
    rs2Source.build()

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

    when (state === State.IDLE) {
      isAvailable := !activeFlush
    }

    activeFlush := False

    // execution was invalidated while running
    when (activeFlush) {
      reset()
    }

    // when waiting for the result, and it is ready, put in on the bus
    when (state === State.EXECUTING && exeStage.arbitration.isDone && !activeFlush) {
      cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)
      pipeline.withService[ContextService](
        context => {
          context.isTransientSecretOfBundle(cdbStream.payload.metadata) := False  // TODO if we want to propagate instead of stall
        }
      )

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex

      for (register <- retirementRegisters.keys) {
        pipeline.withService[ContextService](
          context => {
            // TODO if we want to propagate instead of stall
            if (context.isTransientPipelineReg(register)) {
              dispatchStream.payload.registerMap.element(register) := False
            } else {
              dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
            }
          },
          {
            dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
          }
        )

      }

      when (exeStage.output(pipeline.data.RD_DATA_VALID)) {
        cdbStream.valid := True
      }
      dispatchStream.valid := True

      // Override the assignment of resultCdbMessage to make sure data can be sent in later cycles
      // in case of contention in this cycle
      resultCdbMessage := cdbStream.payload
      resultDispatchMessage := dispatchStream.payload

      cdbWaitingNext := (!cdbStream.ready && cdbStream.valid)
      dispatchWaitingNext := !dispatchStream.ready

      when ((cdbStream.ready || !cdbStream.valid) && dispatchStream.ready) {
        reset()
      } otherwise {
        stateNext := State.BROADCASTING_RESULT
      }
    }

    // if the result is on the buses and it has been acknowledged, make the RS
    // available again
    when (state === State.BROADCASTING_RESULT && !activeFlush) {
      cdbStream.valid := cdbWaiting
      dispatchStream.valid := dispatchWaiting

      when (cdbStream.ready && cdbWaiting) {
        cdbWaitingNext := False
      }

      when (dispatchStream.ready && dispatchWaiting) {
        dispatchWaitingNext := False
      }

      when ((cdbStream.ready || !cdbWaiting) && (dispatchStream.ready || !dispatchWaiting)) {
        reset()
      }
    }
  }

  def execute(): Unit = {
    val dispatchStage = pipeline.issuePipeline.stages.last

    robEntryIndex := rob.pushEntry(
      dispatchStage.output(pipeline.data.RD),
      dispatchStage.output(pipeline.data.RD_TYPE),
      pipeline.getService[LsuService].operationOutput(dispatchStage),
      pipeline.getService[BranchService].isBranch(dispatchStage),
      dispatchStage.output(pipeline.data.PC))

    stateNext := State.EXECUTING
    regs.shift := True

    val rs2Used = dispatchStage.output(pipeline.data.RS2_TYPE) === RegisterType.GPR

    val rs1Id = dispatchStage.output(pipeline.data.RS1)
    val rs2Id = dispatchStage.output(pipeline.data.RS2)

    val (rs1Found, rs1Target) = rob.getValue(rs1Id)
    val (rs2Found, rs2Target) = rob.getValue(rs2Id)

    when (rs1Found) {
      when (rs1Target.valid) {
        pipeline.withService[ContextService](
          context => {
            val secret = context.isTransientSecretOfBundle(rs1Target.payload.metadata)
//            rs1WaitingNext := secret
            rs1Source.priorInstructionNext.valid := False
            // TODO: we should zero these out by default, and then set the jump one here
            //       and i guess add another field to this thing, secret, and only stall if that's true
            when (secret) {
              stateNext := State.WAITING_FOR_ARGS
            }
          },
          {
            rs1Source.priorInstructionNext.valid := False
          }
        )
        regs.setReg(pipeline.data.RS1_DATA, rs1Target.payload.writeValue)
      } otherwise {
        stateNext := State.WAITING_FOR_ARGS
        rs1Source.priorInstructionNext.push(rs1Target.payload.robIndex)
      }
    } otherwise {
      rs1Source.priorInstructionNext.valid := False
      regs.setReg(pipeline.data.RS1_DATA, dispatchStage.output(pipeline.data.RS1_DATA))
    }

    when (rs2Found) {
      when (rs2Target.valid || !rs2Used) {
        pipeline.withService[ContextService](
          context => {
            val secret = context.isTransientSecretOfBundle(rs2Target.payload.metadata)
//            rs2WaitingNext := secret
            rs2Source.priorInstructionNext.valid := False
            when (secret) {
              stateNext := State.WAITING_FOR_ARGS
            }
          },
          {
            rs2Source.priorInstructionNext.valid := False
          }
        )
        regs.setReg(pipeline.data.RS2_DATA, rs2Target.payload.writeValue)
      } otherwise {
        stateNext := State.WAITING_FOR_ARGS
        rs2Source.priorInstructionNext.push(rs2Target.payload.robIndex)
      }
    } otherwise {
      rs2Source.priorInstructionNext.valid := False
      regs.setReg(pipeline.data.RS2_DATA, dispatchStage.output(pipeline.data.RS2_DATA))
    }
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
