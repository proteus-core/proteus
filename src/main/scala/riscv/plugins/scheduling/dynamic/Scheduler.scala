package riscv.plugins.scheduling.dynamic

import riscv._

import spinal.core._

class Scheduler(val robSize: Int = 8) extends Plugin[DynamicPipeline] with IssueService {
  class RegisterStatus extends Bundle {
    val busy = Bool()
    val reorder = UInt(log2Up(robSize) bits)
  }

  class Rob extends Area {
    class Entry extends Bundle {
      val ready = Bool()
      val dest = UInt(log2Up(config.numRegs) bits)
      val value = UInt(config.xlen bits)
    }

    assert(isPow2(robSize))

    private val entries = {
      val defaultEntry = (new Entry).getZero
      Vec.fill(robSize)(Reg(new Entry).init(defaultEntry))
    }

    private val start, end = Reg(UInt(log2Up(robSize) bits)).init(0)
    private val full = Reg(Bool()).init(False)

    def isFull: Bool = full
    def isAvailable: Bool = !isFull

    def push(dest: UInt): UInt = {
      val entry = entries(end)
      entry.ready := False
      entry.dest := dest
      entry.value := U(0, config.xlen bits)

      val newEnd = end + 1
      end := newEnd

      when (newEnd === start) {
        full := True
      }

      end
    }
  }

  class ReservationStation(stage: Stage) {
    private val regs = pipeline.pipelineRegs(stage)
    regs.shift := False

    private val (robEntryIn, robEntryOut) = regs.addReg(data.ROB_ENTRY)
    robEntryIn := 0

    stage.arbitration.isStalled := False

    val valid = Reg(Bool()).init(False)
    stage.arbitration.isValid := valid

    when (stage.arbitration.isDone) {
      valid := False
    }

    def isAvailable: Bool = stage.arbitration.isAvailable

    def execute(robEntry: UInt): Unit = {
      robEntryIn := robEntry
      regs.shift := True
      valid := True
    }
  }

  class Data {
    object DEST_FU extends PipelineData(Bits(pipeline.exeStages.size bits))
    object ROB_ENTRY extends PipelineData(UInt(log2Up(robSize) bits))
  }

  lazy val data = new Data

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure {config =>
      config.addDefault(data.DEST_FU, B(0))
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val reservationStations = pipeline.exeStages.map(new ReservationStation(_))
      val rob = new Rob

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
            val robEntry = rob.push(dispatchStage.output(pipeline.data.RD))
            rs.execute(robEntry)
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

    pipeline.getService[DecoderService].configure {config =>
      var fuMask = 0

      for (exeStage <- pipeline.exeStages.reverse) {
        val nextBit = if (stages.contains(exeStage)) 1 else 0
        fuMask = (fuMask << 1) | nextBit
      }

      config.addDecoding(opcode, Map(data.DEST_FU -> B(fuMask)))
    }
  }
}
