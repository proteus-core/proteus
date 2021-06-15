package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.{Stream, StreamArbiterFactory}

class Scheduler(val robCapacity: Int = 8) extends Plugin[DynamicPipeline] with IssueService {
  val robIndexBits = log2Up(robCapacity) bits

  case class CdbMessage() extends Bundle {
    val robIndex = UInt(robIndexBits)
    val value = UInt(config.xlen bits)
  }

  trait CdbListener {
    def onCdbMessage(cdbMessage: CdbMessage)
  }

  class RegisterFile extends Area { // TODO: not needed?
    val registerArray = Vec.fill(config.numRegs)(Reg(UInt(config.xlen bits)).init(0))

    def getValue(regId: UInt): UInt = {
      registerArray(regId)
    }

    def updateValue(regId: UInt, value: UInt): Unit = {
      registerArray(regId) := value
    }
  }

  object RobEntryType extends SpinalEnum {
    val BRANCH, STORE, REGISTER = newElement()
  }

  case class RobEntry() extends Bundle {
    val instructionType = RobEntryType()
    val destination = UInt(config.xlen bits)
    val value = UInt(config.xlen bits)
    val ready = Bool()
  }

  class Rob(registerFile: RegisterFile) extends Area with CdbListener {
    val robEntries = Vec.fill(robCapacity)(RegInit(RobEntry().getZero))
    val oldestIndex = Reg(UInt(robIndexBits)).init(0)
    val newestIndex = Reg(UInt(robIndexBits)).init(0)
    private val isFull = RegInit(False)
    private val willRetire = False
    val isAvailable = !isFull || willRetire

    val pushInCycle = Bool()
    pushInCycle := False
    val pushedEntry = RobEntry()
    pushedEntry := RobEntry().getZero

    def nextIndex(index: UInt): UInt = {
      val next = UInt()
      when (index + 1 === robCapacity) {
        next := 0
      } otherwise {
        next := index + 1
      }
      next
    }

    def isValidIndex(index: UInt): Bool = {
      val ret = Bool()
      when ((oldestIndex === newestIndex && !isFull) || index >= robCapacity) { // initial setting
        ret := False
      } elsewhen (oldestIndex === newestIndex) { // rob is full
        ret := True
      } elsewhen (newestIndex > oldestIndex) { // normal order
        ret := index >= oldestIndex && index < newestIndex
      } otherwise { // wrapping
        ret := index >= oldestIndex || index < newestIndex
      }
      ret
    }

    def indexForNth(nth: UInt): UInt = {
      val index = UInt(config.xlen bits)
      val adjusted = UInt(config.xlen bits)
      index := (nth + oldestIndex).resized
      when (index >= robCapacity) {
        adjusted := index - robCapacity
      } otherwise {
        adjusted := index
      }
      adjusted
    }

    def pushEntry(instructionType: SpinalEnumElement[RobEntryType.type], destination: UInt): UInt = {
      pushInCycle := True
      pushedEntry.ready := False
      pushedEntry.instructionType := instructionType
      pushedEntry.destination := destination.resized
      newestIndex
    }

    def getValue(regId: UInt): (Bool, Bool, UInt, UInt) = {
      val found = Bool()
      val ready = Bool()
      val value = UInt(config.xlen bits)
      val ix = UInt(robIndexBits)
      found := False
      ready := False
      value := 0
      ix := 0

      // loop through valid values and return the freshest if present
      for (nth <- 0 until robCapacity) {
        val index = indexForNth(nth)
        val entry = robEntries(index.resized)
        when (isValidIndex(index) && entry.destination === regId) {
          found := True
          ready := entry.ready
          value := entry.value
          ix := index.resized
        }
      }
      (found, ready, value, ix)
    }

    override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
      robEntries(cdbMessage.robIndex).value := cdbMessage.value
      robEntries(cdbMessage.robIndex).ready := True
    }

    def build(): Unit = {
      val oldestEntry = robEntries(oldestIndex)
      val updatedOldestIndex = UInt(robIndexBits)
      updatedOldestIndex := oldestIndex
      val isEmpty = oldestIndex === newestIndex && !isFull
      when (!isEmpty && oldestEntry.ready) {
        when (oldestEntry.instructionType === RobEntryType.REGISTER) {
          registerFile.updateValue(oldestEntry.destination.resized, oldestEntry.value)
        }
        updatedOldestIndex := nextIndex(oldestIndex)
        oldestIndex := updatedOldestIndex
        willRetire := True
        isFull := False
      }

      when (pushInCycle) {
        robEntries(newestIndex) := pushedEntry
        val updatedNewest = nextIndex(newestIndex)
        newestIndex := updatedNewest
        when (updatedOldestIndex === updatedNewest) {
          isFull := True
        }
      }
    }
  }

  class Cdb(reservationStations: Seq[ReservationStation], rob: Rob) extends Area {
    val inputs = Vec(Stream(HardType(CdbMessage())), reservationStations.size)

    val arbitratedInputs = StreamArbiterFactory.roundRobin.on(inputs)

    def build(): Unit = {
      when (arbitratedInputs.valid) {
        arbitratedInputs.ready := True
        val listeners = reservationStations :+ rob
        for (listener <- listeners) {
          listener.onCdbMessage(arbitratedInputs.payload)
        }
      } otherwise {
        arbitratedInputs.ready := False
      }
    }
  }

  class ReservationStation(exeStage: Stage, registerFile: RegisterFile, rob: Rob) extends Area with CdbListener {
    setPartialName(s"RS_${exeStage.stageName}")

    private val rs1RobIndexNext, rs2RobIndexNext = UInt(robIndexBits)
    private val rs1WaitingNext, rs2WaitingNext = Bool()

    private val rs1RobIndex = RegNext(rs1RobIndexNext).init(0)
    private val rs2RobIndex = RegNext(rs2RobIndexNext).init(0)
    private val rs1Waiting = RegNext(rs1WaitingNext).init(False)
    private val rs2Waiting = RegNext(rs2WaitingNext).init(False)

    private val robEntryIndex = Reg(UInt(robIndexBits)).init(0)

    private object State extends SpinalEnum {
      val IDLE, WAITING_FOR_ARGS, EXECUTING, BROADCASTING_RESULT = newElement()
    }

    private val stateNext = State()
    private val state = RegNext(stateNext).init(State.IDLE)

    val resultCdbMessage = Reg(CdbMessage())

    val cdbStream = Stream(HardType(CdbMessage()))

    private val regs = pipeline.pipelineRegs(exeStage)

    val isAvailable = Bool()

    override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
      val currentRs1Waiting, currentRs2Waiting = Bool()
      val currentRs1RobIndex, currentRs2RobIndex = UInt(robIndexBits)

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
          regs.setReg(pipeline.data.RS1_DATA, cdbMessage.value)
        }

        when (currentRs2Waiting && cdbMessage.robIndex === currentRs2RobIndex) {
          rs2Waiting := False
          r1w := False
          regs.setReg(pipeline.data.RS2_DATA, cdbMessage.value)
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

      stateNext := state

      resultCdbMessage.robIndex := robEntryIndex
      resultCdbMessage.value := 0

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

      // when waiting for the result, and it is ready, put in on the bus
      when (state === State.EXECUTING && exeStage.arbitration.isDone) {
        val rd = exeStage.output(pipeline.data.RD_DATA)
        resultCdbMessage.value := rd

        // Override the assignment of returnBundle to stream.payload to make
        // sure it's available this cycle (since returnBundle is a Reg)
        cdbStream.payload.robIndex := robEntryIndex
        cdbStream.payload.value := rd
        cdbStream.valid := True

        when (cdbStream.ready) {
          isAvailable := True
          stateNext := State.IDLE
        } otherwise {
          stateNext := State.BROADCASTING_RESULT
        }
      }

      // if the result is on the bus and it has been acknowledged, make the RS
      // available again
      when (state === State.BROADCASTING_RESULT) {
        cdbStream.valid := True

        when (cdbStream.ready) {
          isAvailable := True
          stateNext := State.IDLE
        }
      }
    }

    def execute(robIndex: UInt): Unit = {
      stateNext := State.EXECUTING
      robEntryIndex := robIndex
      regs.shift := True

      val dispatchStage = pipeline.issuePipeline.stages.last
      val immUsed = dispatchStage.output(pipeline.data.IMM_USED)
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
        regs.setReg(pipeline.data.RS1_DATA, registerFile.getValue(rs1Id))
      }

      when (rs2Found) {
        when (rs2Valid || immUsed) {
          rs2WaitingNext := False
          regs.setReg(pipeline.data.RS2_DATA, rs2Value)
        } otherwise {
          stateNext := State.WAITING_FOR_ARGS
          rs2WaitingNext := True
          rs2RobIndexNext := rs2Index
        }
      } otherwise {
        rs2WaitingNext := False
        regs.setReg(pipeline.data.RS2_DATA, registerFile.getValue(rs2Id))
      }
    }
  }

  class Data {
    object DEST_FU extends PipelineData(Bits(pipeline.exeStages.size bits))

    object ROB_ENTRY extends PipelineData(UInt(robIndexBits))
  }

  lazy val data = new Data

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(data.DEST_FU, B(0))
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val registerFile = new RegisterFile
      val rob = new Rob(registerFile)
      val reservationStations = pipeline.exeStages.map(stage => new ReservationStation(stage, registerFile, rob))
      val cdb = new Cdb(reservationStations, rob)
      rob.build()
      cdb.build()
      for ((rs, index) <- reservationStations.zipWithIndex) {
        rs.build()
        rs.cdbStream >> cdb.inputs(index)
      }

      // Dispatch
      val dispatchStage = pipeline.issuePipeline.stages.last
      dispatchStage.arbitration.isStalled := False

      when (dispatchStage.arbitration.isValid && dispatchStage.arbitration.isReady) {
        val fuMask = dispatchStage.output(data.DEST_FU)

        var context = when (fuMask === 0) {
          // TODO Illegal instruction
        }

        for ((rs, index) <- reservationStations.zipWithIndex) {
          context = context.elsewhen (fuMask(index) && rs.isAvailable && rob.isAvailable) {
            val robIndex = rob.pushEntry(RobEntryType.REGISTER, dispatchStage.output(pipeline.data.RD))
            rs.execute(robIndex)
          }
        }

        context.otherwise {
          dispatchStage.arbitration.isStalled := True
        }
      }
    }
  }

  override def setDestinations(opcode: MaskedLiteral, stages: Set[Stage]): Unit = {
    for (stage <- stages) {
      assert(pipeline.exeStages.contains(stage),
        s"Stage ${stage.stageName} is not an execute stage")
    }

    pipeline.getService[DecoderService].configure { config =>
      var fuMask = 0

      for (exeStage <- pipeline.exeStages.reverse) {
        val nextBit = if (stages.contains(exeStage)) 1 else 0
        fuMask = (fuMask << 1) | nextBit
      }

      config.addDecoding(opcode, Map(data.DEST_FU -> B(fuMask)))
    }
  }
}
