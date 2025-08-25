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
        when(unknownStore) {
          lsu.stlSpeculation(storedMessage.registerMap) := True
          lsu.stlSpeculation(targetRobEntry.registerMap) := True
          pipeline.serviceOption[SpeculationService] foreach { spec =>
            spec.isSpeculativeMD(storedMessage.registerMap) := True
            spec.isSpeculativeMD(targetRobEntry.registerMap) := True
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

    lsu.psfMisspeculation(resultCdbMessage.metadata) := False
    cdbStream.valid := False
    cdbStream.payload := resultCdbMessage

    for (register <- retirementRegisters.keys) {
      loadStage.input(register) := storedMessage.registerMap.element(register)
    }

    rdbStream.valid := False
    rdbStream.payload := outputCache

    val address = lsu.addressOfBundle(storedMessage.registerMap)

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
        // for loads that did not have a PSF prediction, set the correct address to prevent a pipeline flush
        lsu.psfAddress(cdbStream.payload.metadata) := address

        pipeline.serviceOption[SpeculationService] foreach { spec =>
          spec.isSpeculativeMD(cdbStream.metadata) := spec.isSpeculativeMD(
            storedMessage.registerMap
          )
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
