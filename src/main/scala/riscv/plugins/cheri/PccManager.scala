package riscv.plugins.cheri

import riscv._

import spinal.core._

import scala.collection.mutable

class PccManager(branchStage: Stage)(implicit context: Context)
  extends Plugin[StaticPipeline] with PccService {
  private class PccData extends PackedCapability(hasOffset = false)
  private object PccData {
    def apply(): PccData = new PccData
    def Root: PccData = PccData().assignRoot()
  }

  private object Data {
    object PCC extends PipelineData(PccData())
    object CJALR extends PipelineData(Bool())
  }

  private def checkJumpTarget(stage: Stage, address: UInt): Bool = {
    val ok = True
    val targetPccInfo = getTargetPcc(stage)

    def except(cause: ExceptionCause) = {
      val handler = pipeline.service[ExceptionHandler]
      handler.except(stage, cause, targetPccInfo.capIdx)
      ok := False
    }

    val targetPcc = targetPccInfo.value

    when (!targetPcc.tag) {
      except(ExceptionCause.TagViolation)
    } elsewhen (targetPcc.isSealed) {
      except(ExceptionCause.SealViolation)
    } elsewhen (!targetPcc.perms.execute) {
      except(ExceptionCause.PermitExecuteViolation)
    } elsewhen (address + 4 > targetPcc.top) {
      except(ExceptionCause.LengthViolation)
    } elsewhen (address < targetPcc.base) {
      except(ExceptionCause.LengthViolation)
    }

    ok
  }

  private trait TargetPccInfo {
    def value: PccData
    def capIdx: CapIdx
  }

  private val targetPccInfos = mutable.Map[Stage, TargetPccInfo]()

  private def getTargetPcc(stage: Stage): TargetPccInfo = {
    targetPccInfos(stage)
  }

  private def setTargetPcc(stage: Stage, pcc: Capability, capIdx: CapIdx) = {
    val info = getTargetPcc(stage)
    info.value.assignFrom(pcc)
    info.capIdx := capIdx
  }

  private def getCurrentPcc(stage: Stage): PccData = {
    val pcc = PccData()
    pcc.assignFrom(stage.value(Data.PCC))
    pcc
  }

  override def getPcc(stage: Stage): Capability = {
    val pcc = PackedCapability()
    pcc.assignFrom(getCurrentPcc(stage))
    pcc.offset := stage.input(pipeline.data.PC)
    pcc
  }

  def jump(stage: Stage, pcc: Capability, capIdx: CapIdx): Unit = {
    setTargetPcc(stage, pcc, capIdx)

    val pc = UInt(config.xlen bits)
    pc := pcc.offset
    pc.lsb := False
    pipeline.service[JumpService].jump(stage, pc)
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.CJALR -> False
      ))

      config.addDecoding(Opcodes.CJALR, InstructionType.R_CxC, Map(
        Data.CJALR -> True
      ))
    }

    // PCC is implemented as a pipeline register that travels along with PC. For
    // each stage, an output is added for the value of the next PCC. When a
    // stage requests a jump, this output is used to update PCC for the target
    // instruction.
    for (stage <- pipeline.stages) {
      stage plug new Area {
        val targetPcc = new Area with TargetPccInfo {
          override val value = out(PccData())
          value := getCurrentPcc(stage)
          override val capIdx = CapIdx.scr(ScrIndex.PCC)
        }

        targetPccInfos(stage) = targetPcc
      }
    }

    // Register PCC as a payload of PC. JumpService will make sure PCC is
    // updated whenever PC is through these callbacks.
    pipeline.service[JumpService] addPcPayload new PcPayload[PccData] {
      override def dataType = HardType(PccData())
      override def initValue = PccData.Root
      override def get(stage: Stage) = getTargetPcc(stage).value
      override def set(stage: Stage, value: PccData) = {
        stage.input(Data.PCC) := value
      }
    }

    // Checking permissions on PCC is done at two locations.
    // 1) While fetching instructions (combined with offsetting PC via PCC).
    // This will catch PCC violations caused by normal (non-jump) PC updates.
    pipeline.service[FetchService] setAddressTranslator new FetchAddressTranslator {
      override def translate(stage: Stage, address: UInt): UInt = {
        val pcc = getCurrentPcc(stage)
        val fetchAddress = pcc.base + address
        checkJumpTarget(stage, fetchAddress)
        fetchAddress
      }
    }

    pipeline.service[JumpService] onJump { (stage, prevPc, nextPc, jumpType) =>
      jumpType match {
        // 2) While executing jumps. Note that this violation would ultimately
        // also be caught by the logic added to the fetch stage but it seems to
        // make more sense to fault the jump instruction instead of the target.
        case JumpType.Normal => {
          val pccInfo = getTargetPcc(stage)
          checkJumpTarget(stage, pccInfo.value.base + nextPc)
        }
        // No permission checks are done on traps since the new trap would fail
        // again for the same reason. There seem to be two ways of handling
        // this:
        // 1) Generate an exception when a capability that would cause a trap is
        //    is written to MTCC.
        // 2) Generate a full system reset when a trap is taken with an MTCC
        //    that would cause another trap.
        // Since an illegal MTCC value is almost certainly a bug, and there seem
        // to be no use-cases for trying to recover from it, we opt for option
        // 2) for now.
        // TODO: actually implement a full reset.
        case JumpType.Trap => {
          val scrService = pipeline.service[ScrService]
          val mtcc = scrService.getScr(stage, ScrIndex.MTCC)
          val mepcc = scrService.getScr(stage, ScrIndex.MEPCC)

          setTargetPcc(stage, mtcc.read(), CapIdx.scr(ScrIndex.MTCC))
          mepcc.write(getPcc(stage))
        }
        // No permission checks are done on MRET either because the target
        // address comes from the source of the trap and it seems to make more
        // sense to generate a violation there.
        case JumpType.TrapReturn => {
          val scrService = pipeline.service[ScrService]
          val mepcc = scrService.getScr(stage, ScrIndex.MEPCC)
          setTargetPcc(stage, mepcc.read(), CapIdx.scr(ScrIndex.MEPCC))
        }
      }
    }

    pipeline.service[ScrService] registerScr(ScrIndex.MTCC, 0x305, new Area with Scr {
      override val needAsr: Boolean = true
      private val mtcc = Reg(PackedCapability()).init(PackedCapability.Root)
      override def read(): Capability = mtcc
      override def write(value: Capability): Unit = mtcc.assignFrom(value)
    })

    pipeline.service[ScrService] registerScr(ScrIndex.MEPCC, 0x341, new Area with Scr {
      override val needAsr: Boolean = true
      private val mepcc = Reg(PackedCapability()).init(PackedCapability.Root)
      override def read(): Capability = mepcc
      override def write(value: Capability): Unit = mepcc.assignFrom(value)
    })
  }

  override def build(): Unit = {
    branchStage plug new Area {
      import branchStage._

      when (arbitration.isValid && value(Data.CJALR)) {
        arbitration.rs1Needed := True

        when (!arbitration.isStalled) {
          val targetPcc = value(context.data.CS1_DATA)
          val capIdx = CapIdx.gpcr(value(pipeline.data.RS1))
          jump(branchStage, targetPcc, capIdx)

          val cd = PackedCapability()
          cd.assignFrom(getCurrentPcc(branchStage))
          cd.offset := input(pipeline.data.NEXT_PC)
          output(context.data.CD_DATA).assignFrom(cd)
          output(pipeline.data.RD_DATA_VALID) := True
        }
      }
    }

    if (config.debug) {
      // Add a debug pipeline register for the absolute PC. Since PC is relative to PCC, it's
      // difficult to interpret the waveform when PCC.base != 0. ABS_PC to the rescue!
      object ABS_PC extends PipelineData(UInt(config.xlen bits))

      pipeline.fetchStage plug {
        pipeline.fetchStage.output(ABS_PC) := getPcc(pipeline.fetchStage).address
      }

      // This forces ABS_PC to be propagated through the whole pipeline.
      pipeline.retirementStage.output(ABS_PC)
    }
  }
}
