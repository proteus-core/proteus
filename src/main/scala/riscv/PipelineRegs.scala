package riscv

import spinal.core._

import scala.collection.mutable

class PipelineRegs(stage: Stage) extends Component {
  private case class RegInfo[T <: Data](reg: T, input: T, output: T, shift: Bool)
  private val regs = mutable.Map[PipelineData[Data], RegInfo[Data]]()

  definitionName = s"PipelineRegs_${stage.stageName}"
  val shift = in Bool ()

  def addReg[T <: Data](data: PipelineData[T]): (T, T) = rework {
    val input = in(data.dataType())
    val output = out(data.dataType())
    val regShift = in(Bool())
    parent.rework { regShift := False }
    val reg = RegNextWhen(input, shift || regShift).init(input.getZero)
    output := reg

    input.setName(s"in_${data.name}")
    output.setName(s"out_${data.name}")
    reg.setName(s"reg_${data.name}")
    regShift.setName(s"shift_${data.name}")

    regs(data.asInstanceOf[PipelineData[Data]]) = RegInfo(reg, input, output, regShift)

    (input, output)
  }

  def setReg[T <: Data](data: PipelineData[T], value: T): Unit = {
    val regInfo =
      regs(data.asInstanceOf[PipelineData[Data]]).asInstanceOf[RegInfo[T]]

    regInfo.input := value
    regInfo.shift := True
  }
}
