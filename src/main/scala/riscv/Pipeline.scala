package riscv

import spinal.core._

import scala.collection.mutable
import scala.reflect.ClassTag

class PipelineRegs(stage: Stage) extends Component {
  private case class RegInfo[T <: Data](reg: T, input: T, output: T, shift: Bool)
  private val regs = mutable.Map[PipelineData[Data], RegInfo[Data]]()

  definitionName = s"PipelineRegs_${stage.stageName}"
  val shift = in Bool()

  def addReg[T <: Data](data: PipelineData[T]): (T, T) = rework {
    val input = in(data.dataType())
    val output = out(data.dataType())
    val regShift = in(Bool())
    parent.rework {regShift := False}
    val reg = RegNextWhen(input, shift || regShift).init(input.getZero)
    output := reg

    input.setName(s"in_${data.name}")
    output.setName(s"out_${data.name}")
    regShift.setName(s"shift_${data.name}")

    regs(data.asInstanceOf[PipelineData[Data]]) =
      RegInfo(reg, input, output, regShift)

    (input, output)
  }

  def setReg[T <: Data](data: PipelineData[T], value: T): Unit = {
    val regInfo =
      regs(data.asInstanceOf[PipelineData[Data]]).asInstanceOf[RegInfo[T]]

    regInfo.input := value
    regInfo.shift := True
  }
}

class Pipeline(val config: Config, plugins: Seq[Plugin]) extends Component {
  val fetch = new Stage("IF")
  val decode = new Stage("ID")
  val execute = new Stage("EX")
  val memory = new Stage("MEM")
  val writeback = new Stage("WB")
  val stages = Seq(fetch, decode, execute, memory, writeback)
  val pipelineRegs = stages.init.map(stage => {
    val regs = new PipelineRegs(stage)
    regs.setName(s"pipelineRegs_${stage.stageName}")
    stage -> regs
  }).toMap

  val data = new StandardPipelineData(config)

  for (plugin <- plugins) {
    plugin.setup(this)
  }

  for (plugin <- plugins) {
    plugin.build(this)
  }

  // Debugging signals to ensure that PC and IR are passed through the whole
  // pipeline.
  val retiredPc = UInt(config.xlen bits)
  retiredPc := stages.last.output(data.PC)
  val retiredIr = UInt(config.xlen bits)
  retiredIr := stages.last.output(data.IR)

  private class PipelineDataInfo {
    var firstOutputStageId = Int.MaxValue
    var lastOutputStageId = -1
    var firstInputStageId = Int.MaxValue
    var lastInputStageId = -1

    def addOutputStageId(id: Int) = {
      firstOutputStageId = Math.min(firstOutputStageId, id)
      lastOutputStageId = Math.max(lastOutputStageId, id)
    }

    def addInputStageId(id: Int) = {
      firstInputStageId = Math.min(firstInputStageId, id)
      lastInputStageId = Math.max(lastInputStageId, id)
    }

    def isUsedAsInput = lastInputStageId >= 0
  }

  private val pipelineDataInfoMap =
    mutable.Map[PipelineData[_ <: Data], PipelineDataInfo]()

  for ((stage, stageId) <- stages.zipWithIndex) {
    for (valueData <- stage.lastValues.keys) {
      if (!stage.outputs.contains(valueData)) {
        stage.input(valueData)
      }
    }

    for (inputData <- stage.inputs.keys) {
      val info =
        pipelineDataInfoMap.getOrElseUpdate(inputData, new PipelineDataInfo)
      info.addInputStageId(stageId)
    }

    for (outputData <- stage.outputs.keys) {
      val info =
        pipelineDataInfoMap.getOrElseUpdate(outputData, new PipelineDataInfo)
      info.addOutputStageId(stageId)
    }
  }

  for ((data, info) <- pipelineDataInfoMap) {
    if (info.isUsedAsInput && info.firstInputStageId > 0) {
      assert(
        info.firstOutputStageId <= info.firstInputStageId,
        s"Pipeline register ${data.name} used as input in stage " +
          s"${stages(info.firstInputStageId).stageName} before being first " +
          s"output in stage ${stages(info.firstOutputStageId).stageName}")
    }

    var firstStageId = Math.min(info.firstInputStageId, info.firstOutputStageId)
    var lastStageId = Math.max(info.lastInputStageId, info.lastOutputStageId)

    for (outputStageId <- firstStageId until lastStageId) {
      val outputStage = stages(outputStageId)
      val inputStage = stages(outputStageId + 1)
      val input = inputStage.input(data)// := outputStage.output(data)
      val output = outputStage.output(data)
      val outputStageRegs = pipelineRegs(outputStage)
      val (regInput, regOutput) = outputStageRegs.addReg(data)
      regInput := output
      input := regOutput
    }
  }

  for (stage <- stages) {
    stage.connectOutputDefaults()
    stage.connectLastValues()
  }

  plugins.foreach(_.finish(this))

  def getService[T](implicit tag: ClassTag[T]): T = {
    val services = plugins.filter(_ match {
      case _: T => true
      case _    => false
    })

    assert(services.length == 1)
    services.head.asInstanceOf[T]
  }

  def hasService[T](implicit tag: ClassTag[T]): Boolean = {
    plugins.exists(_ match {
      case _: T => true
      case _    => false
    })
  }

  def getImplementedExtensions: Seq[Extension] = {
    Extension(config.baseIsa) +: plugins.flatMap(_.getImplementedExtensions)
  }
}
