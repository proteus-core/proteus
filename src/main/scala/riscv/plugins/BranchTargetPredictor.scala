package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.Counter

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
      counter.clear()

      def recordJump(pc: UInt, target: UInt): Unit = {
        val index = counter.value
        entries(index).pc := pc(storedPcBitLength - 1 downto 0)
        entries(index).target := target
        counter.increment()
      }

      when(jumpIo.mispredicted) {
        recordJump(jumpIo.currentPc, jumpIo.target)
      }

      predictIo.predictedAddress.payload.assignDontCare()
      predictIo.predictedAddress.valid := False

      for (i <- 0 until numEntries) {
        val entry = entries(i)

        when(entry.pc === predictIo.currentPc(storedPcBitLength - 1 downto 0)) {
          predictIo.predictedAddress.payload := entry.target
          predictIo.predictedAddress.valid := True
        }
      }
    }
  }
}
