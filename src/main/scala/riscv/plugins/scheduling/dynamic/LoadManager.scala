package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

class LoadManager(
    pipeline: Pipeline,
    loadStage: Stage,
    rob: ReorderBuffer,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area
    with Resettable {
  setPartialName(s"LM_${loadStage.stageName}")

  private val lsu = pipeline.service[LsuService]

  val storedMessage: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)
  val outputCache: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)
  val rdbStream: Stream[RdbMessage] = Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))
  )
  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(metaRegisters, rob.indexBits)))
  private val resultCdbMessage = RegInit(CdbMessage(metaRegisters, rob.indexBits).getZero)
  val rdbWaitingNext, cdbWaitingNext = Bool()
  val rdbWaiting: Bool = RegNext(rdbWaitingNext).init(False)
  val cdbWaiting: Bool = RegNext(cdbWaitingNext).init(False)

  private val discardResult = RegInit(False)

  private val activeFlush = Bool()

  private object State extends SpinalEnum {
    val IDLE, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)
  val isAvailable: Bool = Bool()

  def receiveMessage(rdbMessage: RdbMessage): Bool = {
    val ret = Bool()
    ret := False
    val address = lsu.addressOfBundle(rdbMessage.registerMap)
    val (matchingStore, unknownStore) = rob.hasPendingStoreForEntry(rdbMessage.robIndex, address)

    val invalidatingNow = rob.softResetThisCycle && rob.relativeIndexForAbsolute(
      rdbMessage.robIndex
    ) > rob.relativeIndexForAbsolute(rob.currentSoftResetTrigger.payload)

    if (config.stlSpec) {
      val targetRobEntry = rob.robEntries(rdbMessage.robIndex)
      when(isAvailable && !matchingStore && !invalidatingNow && !targetRobEntry.preventSsb) {
        ret := True
        stateNext := State.EXECUTING
        storedMessage := rdbMessage

        // TODO: this is very ugly
        lsu.addressOfBundle(targetRobEntry.registerMap) := address
        lsu.addressValidOfBundle(targetRobEntry.registerMap) := True

        /** This is speculative store bypass (SSB): When the ROB contains a store instruction with
          * an unknown address, we speculate that it will not overlap with the address of the
          * current load, so we issue this load but mark it as SSB speculative for validation later
          * once the store address is available.
          *
          * To avoid the speculation remaining undetected if the load takes longer than the address
          * resolution of the store, we immediately set the speculation bit in the ROB.
          */
        when(unknownStore) {
          pipeline.serviceOption[DataSpeculationService] foreach { spec =>
            spec.isSsbSpeculative(storedMessage.registerMap) := True
            spec.isSsbSpeculative(targetRobEntry.registerMap) := True
          }
        }
      }
    } else {
      when(isAvailable && !matchingStore && !unknownStore && !invalidatingNow) {
        ret := True
        stateNext := State.EXECUTING
        storedMessage := rdbMessage
      }
    }
    ret
  }

  def build(): Unit = {
    stateNext := state
    isAvailable := False
    activeFlush := False

    when(state === State.IDLE) {
      isAvailable := !activeFlush
    }

    cdbWaitingNext := cdbWaiting
    rdbWaitingNext := rdbWaiting

    loadStage.arbitration.isValid := state === State.EXECUTING
    loadStage.arbitration.isStalled := False

    // execution was invalidated while running
    when(activeFlush) {
      stateNext := State.IDLE
      discardResult := False
    }

    when(rob.softResetThisCycle && state === State.EXECUTING) {
      when(
        rob.relativeIndexForAbsolute(storedMessage.robIndex) > rob.relativeIndexForAbsolute(
          rob.currentSoftResetTrigger.payload
        )
      ) {
        discardResult := True
      }
    }

    pipeline.serviceOption[DataSpeculationService] foreach { spec =>
      spec.isPsfSpeculative(resultCdbMessage.metadata) := False
    }

    cdbStream.valid := False
    cdbStream.payload := resultCdbMessage

    for (register <- retirementRegisters.keys) {
      loadStage.input(register) := storedMessage.registerMap.element(register)
    }

    rdbStream.valid := False
    rdbStream.payload := outputCache

    when(state === State.EXECUTING && loadStage.arbitration.isDone && !activeFlush) {
      when(discardResult) {
        stateNext := State.IDLE
        discardResult := False
      } otherwise {
        rdbStream.valid := True
        rdbStream.payload.robIndex := storedMessage.robIndex
        rdbStream.payload.willCdbUpdate := True
        for (register <- retirementRegisters.keys) {
          // speculation flags are also propagated here
          rdbStream.payload.registerMap.element(register) := loadStage.output(register)
        }
        outputCache := rdbStream.payload

        cdbStream.valid := True
        cdbStream.payload.writeValue := loadStage.output(pipeline.data.RD_DATA)
        cdbStream.payload.robIndex := storedMessage.robIndex

        pipeline.serviceOption[DataSpeculationService] foreach { spec =>
          spec.isSsbSpeculative(cdbStream.metadata) := spec.isSsbSpeculative(
            storedMessage.registerMap
          )
        }

        pipeline.serviceOption[PipelineTaintService] foreach { tracking =>
          tracking.tainted(cdbStream.metadata) := tracking.tainted(loadStage)
        }

        resultCdbMessage := cdbStream.payload

        rdbWaitingNext := !rdbStream.ready
        cdbWaitingNext := !cdbStream.ready

        when(!rdbStream.ready || !cdbStream.ready) {
          stateNext := State.BROADCASTING_RESULT
        } otherwise {
          stateNext := State.IDLE
          isAvailable := True
        }
      }
    }

    when(state === State.BROADCASTING_RESULT && !activeFlush) {
      // if we already started broadcasting, we will let it complete
      // TODO: this might still take many cycles though
      // once everything else works, experiment with canceling this because why not
      rdbStream.valid := rdbWaiting
      cdbStream.valid := cdbWaiting

      when(rdbStream.ready) {
        rdbWaitingNext := False
      }
      when(cdbStream.ready) {
        cdbWaitingNext := False
      }
      when((rdbStream.ready || !rdbWaiting) && (cdbStream.ready || !cdbWaiting)) {
        stateNext := State.IDLE
        isAvailable := True
        discardResult := False
      }
    }

    // FIXME this doesn't seem the correct place to do this...
    loadStage.connectOutputDefaults()
    loadStage.connectLastValues()
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
