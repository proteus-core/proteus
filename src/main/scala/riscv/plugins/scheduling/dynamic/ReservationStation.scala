package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Stream

trait Resettable {
  def pipelineReset(): Unit
}

class ReservationStation(exeStage: Stage,
                         rob: ReorderBuffer,
                         pipeline: DynamicPipeline,
                         retirementRegisters: DynBundle[PipelineData[Data]],
                         metaRegisters: DynBundle[PipelineData[Data]])
                        (implicit config: Config) extends Area with CdbListener with Resettable {
  setPartialName(s"RS_${exeStage.stageName}")

  private val rs1RobIndexNext, rs2RobIndexNext = UInt(rob.indexBits)
  private val rs1WaitingNext, rs2WaitingNext = Bool()

  private val rs1RobIndex = RegNext(rs1RobIndexNext).init(0)
  private val rs2RobIndex = RegNext(rs2RobIndexNext).init(0)
  private val rs1Waiting = RegNext(rs1WaitingNext).init(False)
  private val rs2Waiting = RegNext(rs2WaitingNext).init(False)

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

  private def contextActive = pipeline.hasService[ContextService]

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Waiting, currentRs2Waiting = Bool()
    val currentRs1RobIndex, currentRs2RobIndex = UInt(rob.indexBits)

    when (state === State.WAITING_FOR_ARGS) {
      currentRs1Waiting := rs1Waiting
      currentRs2Waiting := rs2Waiting
      currentRs1RobIndex := rs1RobIndex
      currentRs2RobIndex := rs2RobIndex
    } otherwise {
      currentRs1Waiting := rs1WaitingNext
      currentRs2Waiting := rs2WaitingNext
      currentRs1RobIndex := rs1RobIndexNext
      currentRs2RobIndex := rs2RobIndexNext
    }

    when (state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Waiting
      val r2w = Bool()
      r2w := currentRs2Waiting

      when (currentRs1Waiting && cdbMessage.robIndex === currentRs1RobIndex) {
        if (contextActive) {
          when (!pipeline.getService[ContextService].isTransientSecretOfBundle(cdbMessage.metadata)) {
            rs1Waiting := False
            r1w := False
          }
        } else {
          rs1Waiting := False
          r1w := False
        }
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when (currentRs2Waiting && cdbMessage.robIndex === currentRs2RobIndex) {
        if (contextActive) {
          when (!pipeline.getService[ContextService].isTransientSecretOfBundle(cdbMessage.metadata)) {
            rs1Waiting := False
            r1w := False
          }
        } else {
          rs2Waiting := False
          r2w := False
        }
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
    rs1RobIndexNext := rs1RobIndex
    rs2RobIndexNext := rs2RobIndex
    rs1WaitingNext := rs1Waiting
    rs2WaitingNext := rs2Waiting

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
      pipeline.getService[ContextService].isTransientSecretOfBundle(cdbStream.payload.metadata) := False  // TODO if we want to propagate instead of stall

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex

      for (register <- retirementRegisters.keys) {
        // TODO if we want to propagate instead of stall
        if (pipeline.getService[ContextService].isTransientPipelineReg(register)) {
          dispatchStream.payload.registerMap.element(register) := False
        } else {
          dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
        }
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
        if (contextActive) {
          val secret = pipeline.getService[ContextService].isTransientSecretOfBundle(rs1Target.payload.metadata)
          rs1WaitingNext := secret
          when (secret) {
            stateNext := State.WAITING_FOR_ARGS
          }
        } else {
          rs1WaitingNext := False
        }
        regs.setReg(pipeline.data.RS1_DATA, rs1Target.payload.writeValue)
      } otherwise {
        stateNext := State.WAITING_FOR_ARGS
        rs1WaitingNext := True
        rs1RobIndexNext := rs1Target.payload.robIndex
      }
    } otherwise {
      rs1WaitingNext := False
      regs.setReg(pipeline.data.RS1_DATA, dispatchStage.output(pipeline.data.RS1_DATA))
    }

    when (rs2Found) {
      when (rs2Target.valid || !rs2Used) {
        if (contextActive) {
          val secret = pipeline.getService[ContextService].isTransientSecretOfBundle(rs2Target.payload.metadata)
          rs2WaitingNext := secret
          when (secret) {
            stateNext := State.WAITING_FOR_ARGS
          }
        } else {
          rs2WaitingNext := False
        }
        regs.setReg(pipeline.data.RS2_DATA, rs2Target.payload.writeValue)
      } otherwise {
        stateNext := State.WAITING_FOR_ARGS
        rs2WaitingNext := True
        rs2RobIndexNext := rs2Target.payload.robIndex
      }
    } otherwise {
      rs2WaitingNext := False
      regs.setReg(pipeline.data.RS2_DATA, dispatchStage.output(pipeline.data.RS2_DATA))
    }
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
