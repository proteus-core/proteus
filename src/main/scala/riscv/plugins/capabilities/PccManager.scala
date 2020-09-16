package riscv.plugins.capabilities

import riscv._

import spinal.core._

import scala.collection.mutable

class PccManager(branchStage: Stage)(implicit context: Context) extends Plugin[StaticPipeline] {
  private class PccData extends PackedCapability(hasOffset = false)
  private object PccData {
    def apply(): PccData = new PccData
    def Root: PccData = PccData().assignRoot()
  }

  private object Data {
    object CJALR extends PipelineData(Bool())
  }

  private def checkJumpTarget(stage: Stage, address: UInt): Bool = {
    val ok = True
    val targetPccInfo = getTargetPcc(stage)

    def except(cause: ExceptionCause) = {
      val handler = pipeline.getService[ExceptionHandler]
      handler.except(stage, cause, targetPccInfo.capIdx)
      ok := False
    }

    val targetPcc = targetPccInfo.value

    when (!targetPcc.tag) {
      except(ExceptionCause.TagViolation)
    } elsewhen (!targetPcc.perms.execute) {
      except(ExceptionCause.PermitExecuteViolation)
    } elsewhen (address + 4 > targetPcc.top) {
      except(ExceptionCause.LengthViolation)
    } elsewhen (address < targetPcc.base) {
      except(ExceptionCause.AccessSystemRegistersViolation)
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
    pcc.assignFrom(stage.value(context.data.PCC))
    pcc
  }

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure {config =>
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
    pipeline.getService[JumpService] addPcPayload new PcPayload[PccData] {
      override def dataType = HardType(PccData())
      override def initValue = PccData.Root
      override def get(stage: Stage) = getTargetPcc(stage).value
      override def set(stage: Stage, value: PccData) = {
        stage.input(context.data.PCC) := value
      }
    }

    // Checking permissions on PCC is done at two locations.
    // 1) While fetching instructions (combined with offsetting PC via PCC).
    // This will catch PCC violations caused by normal (non-jump) PC updates.
    pipeline.getService[FetchService] setAddressTranslator new FetchAddressTranslator {
      override def translate(stage: Stage, address: UInt): UInt = {
        val pcc = getCurrentPcc(stage)
        val fetchAddress = pcc.base + address
        checkJumpTarget(stage, fetchAddress)
        fetchAddress
      }
    }

    // 2) While executing jumps. Note that this violation would ultimately also
    // be caught by the logic added to the fetch stage but it seems to make more
    // sense to fault the jump instruction instead of the target. Also note that
    // we don't check traps since mtvec+mtcc should always be valid.
    pipeline.getService[JumpService] onJump {(stage, prevPc, nextPc, isTrap) =>
      if (!isTrap) {
        val pccInfo = getTargetPcc(stage)
        checkJumpTarget(stage, pccInfo.value.base + nextPc)
      }
    }
  }

  override def build(): Unit = {
    branchStage plug new Area {
      import branchStage._

      when (arbitration.isValid && value(Data.CJALR)) {
        arbitration.rs1Needed := True

        when (!arbitration.isStalled) {
          val targetPcc = value(context.data.CS1_DATA)
          val capIdx = CapIdx.gpcr(value(pipeline.data.RS1))
          setTargetPcc(branchStage, targetPcc, capIdx)

          val targetPc = UInt(config.xlen bits)
          targetPc := targetPcc.offset
          targetPc.lsb := False
          pipeline.getService[JumpService].jump(branchStage, targetPc)

          val cd = PackedCapability()
          cd.assignFrom(getCurrentPcc(branchStage))
          cd.offset := input(pipeline.data.NEXT_PC)
          output(context.data.CD_DATA).assignFrom(cd)
          output(pipeline.data.RD_VALID) := True
        }
      }
    }
  }
}
