package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Stream

class Dispatcher(pipeline: DynamicPipeline,
                 rob: ReorderBuffer,
                 loadManager: LoadManager,
                 retirementRegisters: DynBundle[PipelineData[Data]]) extends Area {
  val rdbStream: Stream[RdbMessage] = Stream(HardType(RdbMessage(retirementRegisters, rob.indexBits)))
  val storedMessage: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)

  private object State extends SpinalEnum {
    val IDLE, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)

  def processMessage(rdbMessage: RdbMessage): Bool = {
    val ret = Bool()

    val lsu = pipeline.getService[LsuService]

    when (lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.LOAD) {
      ret := loadManager.receiveMessage(rdbMessage)
    } otherwise {
      when (state === State.BROADCASTING_RESULT) {
        ret := False
      } otherwise {
        storedMessage := rdbMessage
        rdbStream.payload := rdbMessage
        rdbStream.valid := True
        when (!rdbStream.ready) {
          stateNext := State.BROADCASTING_RESULT
        }
        ret := True
      }
    }
    ret
  }

  def build(): Unit = {
    rdbStream.payload := storedMessage
    rdbStream.valid := False

    stateNext := state

    when (state === State.BROADCASTING_RESULT) {
      rdbStream.valid := True
      when (rdbStream.ready) {
        stateNext := State.IDLE
      }
    }
  }
}
