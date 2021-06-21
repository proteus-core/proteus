package riscv.plugins.scheduling.dynamic

import riscv.Config
import spinal.core._
import spinal.lib.{Stream, StreamArbiterFactory}

case class CdbMessage(robIndexBits: BitCount)(implicit config: Config) extends Bundle {
  val robIndex = UInt(robIndexBits)
  val value = UInt(config.xlen bits)
}

trait CdbListener {
  def onCdbMessage(cdbMessage: CdbMessage)
}

class CommonDataBus(reservationStations: Seq[ReservationStation], rob: ReorderBuffer)(implicit config: Config) extends Area {
  val inputs = Vec(Stream(HardType(CdbMessage(rob.indexBits))), reservationStations.size)

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
