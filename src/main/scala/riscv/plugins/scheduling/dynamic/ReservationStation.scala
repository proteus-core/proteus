package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Stream

class ReservationStation(exeStage: Stage,
                         rob: ReorderBuffer,
                         pipeline: DynamicPipeline,
                         retirementRegisters: DynBundle[PipelineData[Data]])
                        (implicit config: Config) extends Area with CdbListener {
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

  private val cdbWaitingNext, dispatchWaitingNext = Bool() // TODO: is this necessary?
  private val cdbWaiting = RegNext(cdbWaitingNext).init(False) // TODO: am i using this? can there be a problem if only one bus has to wait?
  private val dispatchWaiting = RegNext(dispatchWaitingNext).init(False)

  private val resultCdbMessage = Reg(CdbMessage(rob.indexBits))
  private val resultDispatchMessage = Reg(RdbMessage(retirementRegisters, rob.indexBits))

  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(rob.indexBits)))
  val dispatchStream: Stream[RdbMessage] = Stream(HardType(RdbMessage(retirementRegisters, rob.indexBits)))

  private val regs = pipeline.pipelineRegs(exeStage) // TODO: do we need this?

  val isAvailable: Bool = Bool()

  def reset(): Unit = {
    isAvailable := True
    stateNext := State.IDLE
  }

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
        rs1Waiting := False
        r1w := False
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when (currentRs2Waiting && cdbMessage.robIndex === currentRs2RobIndex) {
        rs2Waiting := False
        r2w := False
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

    resultCdbMessage.robIndex := robEntryIndex  // TODO: needed?
    resultCdbMessage.writeValue.assignDontCare

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
      isAvailable := True
    }

    // execution was invalidated while running
    when ((state === State.EXECUTING || state === State.WAITING_FOR_ARGS) && !exeStage.arbitration.isValid) {
      reset()
    }

    // when waiting for the result, and it is ready, put in on the bus
    when (state === State.EXECUTING && exeStage.arbitration.isDone) {
      cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex.resized

      for (register <- retirementRegisters.keys) {
        dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
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
    when (state === State.BROADCASTING_RESULT) {
      cdbStream.valid := cdbWaiting
      dispatchStream.valid := dispatchWaiting

      when (cdbStream.ready && cdbWaiting) {
        cdbWaitingNext := False
      }

      when (dispatchStream.ready && dispatchWaiting) {
        dispatchWaitingNext := False
      }

      when (!cdbWaiting && !dispatchWaiting) {
        // TODO: losing a cycle by only setting isAvailable here
        reset()
      }
    }
  }

  def execute(): Unit = {
    val dispatchStage = pipeline.issuePipeline.stages.last

    robEntryIndex := rob.pushEntry(
      dispatchStage.output(pipeline.data.RD),
      dispatchStage.output(pipeline.data.RD_TYPE),
      pipeline.getService[LsuService].operationOutput(dispatchStage))

    stateNext := State.EXECUTING
    regs.shift := True

    val rs2Used = dispatchStage.output(pipeline.data.RS2_TYPE) === RegisterType.GPR

    val rs1Id = dispatchStage.output(pipeline.data.RS1)
    val rs2Id = dispatchStage.output(pipeline.data.RS2)

    val (rs1Found, rs1Valid, rs1Value, rs1Index) = rob.getValue(rs1Id)
    val (rs2Found, rs2Valid, rs2Value, rs2Index) = rob.getValue(rs2Id)

    when (rs1Found) {
      when (rs1Valid) {
        rs1WaitingNext := False
        regs.setReg(pipeline.data.RS1_DATA, rs1Value)
      } otherwise {
        stateNext := State.WAITING_FOR_ARGS
        rs1WaitingNext := True
        rs1RobIndexNext := rs1Index
      }
    } otherwise {
      rs1WaitingNext := False
      regs.setReg(pipeline.data.RS1_DATA, dispatchStage.output(pipeline.data.RS1_DATA))
    }

    when (rs2Found) {
      when (rs2Valid || !rs2Used) {
        rs2WaitingNext := False
        regs.setReg(pipeline.data.RS2_DATA, rs2Value)
      } otherwise {
        stateNext := State.WAITING_FOR_ARGS
        rs2WaitingNext := True
        rs2RobIndexNext := rs2Index
      }
    } otherwise {
      rs2WaitingNext := False
      regs.setReg(pipeline.data.RS2_DATA, dispatchStage.output(pipeline.data.RS2_DATA))
    }
  }
}
