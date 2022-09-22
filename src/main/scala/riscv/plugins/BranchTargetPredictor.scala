package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class BranchTargetPredictor(
    fetchStage: Stage,
    jumpStage: Stage,
    numEntries: Int = 8,
    storedPcBitLength: Int = 32
) extends BranchTargetPredictorBase(fetchStage, jumpStage) {
  override var predictorComponent: PredictorComponent = null

  case class PredictionEntry() extends Bundle {
    val pc: UInt = UInt(storedPcBitLength bits)
    val target: UInt = UInt(config.xlen bits)
  }

  override def build(): Unit = {
    super.build()

    predictorComponent = pipeline plug new PredictorComponent {
      private val entries = Vec.fill(numEntries)(RegInit(PredictionEntry().getZero))
      private val counter = Counter(numEntries)

      def recordJump(pc: UInt, target: UInt): Unit = {
        val result = findEntryIndex(pc)
        when(result.valid) {
          entries(result.payload).target := target
        } otherwise {
          val index = counter.value
          entries(index).pc := pc(storedPcBitLength - 1 downto 0)
          entries(index).target := target
          counter.increment()
        }
      }

      def findEntryIndex(pc: UInt): Flow[UInt] = {
        val result = Flow(UInt(log2Up(numEntries) bits))
        result.valid := False
        result.payload.assignDontCare()

        for (i <- 0 until numEntries) {
          when(entries(i).pc === pc(storedPcBitLength - 1 downto 0)) {
            result.push(i)
          }
        }
        result
      }

      def findEntry(pc: UInt): Flow[UInt] = {
        val index = findEntryIndex(pc)
        val result = Flow(UInt(config.xlen bits))
        result.valid := False
        result.payload.assignDontCare()

        when(index.valid) {
          result.push(entries(index.payload).target)
        }
        result
      }

      when(jumpIo.mispredicted) {
        recordJump(jumpIo.currentPc, jumpIo.target)
      }

      predictIo.predictedAddress.payload.assignDontCare()
      predictIo.predictedAddress.valid := False

      val result = findEntryIndex(predictIo.currentPc)
      when(result.valid) {
        predictIo.predictedAddress.payload := entries(result.payload).target
        predictIo.predictedAddress.valid := True
      }
    }
  }

  override def predictionForAddress(address: UInt): UInt = {
    val entry = predictorComponent.findEntry(address)
    val result = UInt(config.xlen bits)
    when(entry.valid) {
      result := entry.payload
    } otherwise {
      result := address + 4
    }
    result
  }
}
