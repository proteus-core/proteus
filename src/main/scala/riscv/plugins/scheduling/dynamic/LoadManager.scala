package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Stream

class LoadManager(pipeline: Pipeline,
                  loadStage: Stage,
                  rob: ReorderBuffer,
                  retirementRegisters: DynBundle[PipelineData[Data]])
                 (implicit config: Config) extends Area {
  val storedMessage: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)
  val rdbStream: Stream[RdbMessage] = Stream(HardType(RdbMessage(retirementRegisters, rob.indexBits)))
  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(rob.indexBits)))
  private val resultCdbMessage = Reg(CdbMessage(rob.indexBits))
  val rdbWaiting, cdbWaiting = Bool()

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_STORE, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)
  private val isAvailable = Bool()

  def receiveMessage(rdbMessage: RdbMessage): Bool = { // TODO: can we make sure that this method is only called once per cycle?
    val ret = Bool()
    ret := False
    when (isAvailable) {
      ret := True
      storedMessage := rdbMessage
      val address = pipeline.getService[LsuService].addressOfBundle(rdbMessage.registerMap)
      when (!rob.hasPendingStore(rdbMessage.robIndex, address)) {
        stateNext := State.EXECUTING
      } otherwise {
        stateNext := State.WAITING_FOR_STORE
      }
    }
    ret
  }

  def build(): Unit = {
    stateNext := state
    isAvailable := False

    when (state === State.IDLE) {
      isAvailable := True
    }

    rdbWaiting := False
    cdbWaiting := False

    loadStage.arbitration.isStalled := state === State.WAITING_FOR_STORE
    loadStage.arbitration.isValid := (state === State.WAITING_FOR_STORE) || (state === State.EXECUTING)

    rdbStream.valid := False
    rdbStream.payload := storedMessage

    cdbStream.valid := False
    cdbStream.payload.robIndex := storedMessage.robIndex
    cdbStream.payload.writeValue.assignDontCare()

    resultCdbMessage.robIndex := storedMessage.robIndex
    resultCdbMessage.writeValue.assignDontCare

    for (register <- retirementRegisters.keys) {
      loadStage.input(register) := storedMessage.registerMap.element(register)
    }

    when (state === State.WAITING_FOR_STORE) {
      val address = pipeline.getService[LsuService].addressOfBundle(storedMessage.registerMap)
      when (!rob.hasPendingStore(storedMessage.robIndex, address)) {
        state := State.EXECUTING
      }
    }

    when (state === State.EXECUTING && loadStage.arbitration.isDone) {
      rdbStream.payload.robIndex := storedMessage.robIndex
      rdbStream.valid := True
      for (register <- retirementRegisters.keys) {
        rdbStream.payload.registerMap.element(register) := loadStage.output(register)
      }

      cdbStream.payload.writeValue := loadStage.output(pipeline.data.RD_DATA)
      cdbStream.payload.robIndex := storedMessage.robIndex
      cdbStream.valid := True

      rdbWaiting := !rdbStream.ready
      cdbWaiting := !cdbStream.ready

      when (rdbWaiting || cdbWaiting) {
        stateNext := State.BROADCASTING_RESULT
      } otherwise {
        stateNext := State.IDLE
        isAvailable := True
      }
    }

    when (state === State.BROADCASTING_RESULT) {
      when (rdbStream.ready) {
        rdbWaiting := False
      }
      when (cdbStream.ready) {
        cdbWaiting := False
      }
      when (!rdbWaiting && !cdbWaiting) {
        stateNext := State.IDLE
        isAvailable := True
      }
    }
    
    // FIXME this doesn't seem the correct place to do this...
    loadStage.connectOutputDefaults()
    loadStage.connectLastValues()
  }
}
