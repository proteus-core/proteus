package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Stream

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

  private val activeFlush = Bool()

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_STORE, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)
  val isAvailable: Bool = Bool()

  def receiveMessage(rdbMessage: RdbMessage): Bool = {
    val ret = Bool()
    ret := False
    val address = pipeline.service[LsuService].addressOfBundle(rdbMessage.registerMap)
    val (matchingStore, unknownStore) = rob.hasPendingStoreForEntry(rdbMessage.robIndex, address)

    if (config.stlSpec) {
      when(isAvailable && !matchingStore) {
        ret := True
        stateNext := State.EXECUTING
        storedMessage := rdbMessage
        val targetRobEntry = rob.robEntries(rdbMessage.robIndex)

        // TODO: this is very ugly
        pipeline.service[LsuService].addressOfBundle(targetRobEntry.registerMap) := address
        pipeline.service[LsuService].addressValidOfBundle(targetRobEntry.registerMap) := True
        when(unknownStore) {
          pipeline.service[LsuService].stlSpeculation(storedMessage.registerMap) := True
          pipeline.service[LsuService].stlSpeculation(targetRobEntry.registerMap) := True
          pipeline.serviceOption[SpeculationService] foreach { spec =>
            spec.isSpeculativeMD(storedMessage.registerMap) := True
            spec.isSpeculativeMD(targetRobEntry.registerMap) := True
          }
        }
      }
    } else {
      when(isAvailable && !matchingStore && !unknownStore) {
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

    loadStage.arbitration.isStalled := state === State.WAITING_FOR_STORE
    loadStage.arbitration.isValid := state === State.EXECUTING

    // execution was invalidated while running
    when(activeFlush) {
      stateNext := State.IDLE
    }

    cdbStream.valid := False
    cdbStream.payload := resultCdbMessage

    for (register <- retirementRegisters.keys) {
      loadStage.input(register) := storedMessage.registerMap.element(register)
    }

    rdbStream.valid := False
    rdbStream.payload := outputCache

    val address = pipeline.service[LsuService].addressOfBundle(storedMessage.registerMap)

    when(state === State.WAITING_FOR_STORE && !activeFlush) {
      val (matching, unknown) = rob.hasPendingStoreForEntry(storedMessage.robIndex, address)
      if (config.stlSpec) {
        when(!matching) {
          state := State.EXECUTING
        }
      } else {
        when(!matching && !unknown) {
          state := State.EXECUTING
        }
      }
    }

    when(state === State.EXECUTING && loadStage.arbitration.isDone && !activeFlush) {
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
      pipeline.service[LsuService].psfAddress(cdbStream.payload.metadata) := address

      pipeline.serviceOption[SpeculationService] foreach { spec =>
        spec.isSpeculativeMD(cdbStream.metadata) := spec.isSpeculativeMD(storedMessage.registerMap)
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

    when(state === State.BROADCASTING_RESULT && !activeFlush) {
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
