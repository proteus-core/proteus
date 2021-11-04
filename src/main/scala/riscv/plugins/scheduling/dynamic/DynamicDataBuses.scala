package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

case class CdbMessage(robIndexBits: BitCount)(implicit config: Config) extends Bundle {
  val robIndex: UInt = UInt(robIndexBits)
  val writeValue: UInt = UInt(config.xlen bits)
}

trait CdbListener {
  def onCdbMessage(cdbMessage: CdbMessage)
}

class CommonDataBus(reservationStations: Seq[ReservationStation], rob: ReorderBuffer)
                   (implicit config: Config) extends Area {
  val inputs: Vec[Stream[CdbMessage]] =
    Vec(Stream(HardType(CdbMessage(rob.indexBits))), reservationStations.size + 1) // +1 for loadManager

  private val arbitratedInputs = StreamArbiterFactory.roundRobin.on(inputs)

  def build(): Unit = {
    when (arbitratedInputs.valid) {
      arbitratedInputs.ready := True
      val listeners = reservationStations
      for (listener <- listeners) {
        listener.onCdbMessage(arbitratedInputs.payload)
      }
    } otherwise {
      arbitratedInputs.ready := False
    }
  }
}

class DispatchBus(reservationStations: Seq[ReservationStation],
                  rob: ReorderBuffer,
                  dispatcher: Dispatcher,
                 retirementRegisters: DynBundle[PipelineData[Data]]) extends Area {
  val inputs: Vec[Stream[RdbMessage]] = Vec(Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))), reservationStations.size)
  private val arbitratedInputs = StreamArbiterFactory.roundRobin.on(inputs)

  def build(): Unit = {
    when (arbitratedInputs.valid) {
      arbitratedInputs.ready := dispatcher.processMessage(arbitratedInputs.payload)
    } otherwise {
      arbitratedInputs.ready := False
    }
  }
}

class RobDataBus(rob: ReorderBuffer,
                 retirementRegisters: DynBundle[PipelineData[Data]]) extends Area {
  val inputs: Vec[Stream[RdbMessage]] = Vec(Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))), 2) // size: dispatcher + loadManager
  private val arbitratedInputs = StreamArbiterFactory.roundRobin.on(inputs)

  def build(): Unit = {
    when (arbitratedInputs.valid) {
      arbitratedInputs.ready := True
      rob.onRdbMessage(arbitratedInputs.payload)
    } otherwise {
      arbitratedInputs.ready := False
    }
  }
}
