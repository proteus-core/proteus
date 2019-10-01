package riscv

import spinal.core._

import collection.mutable

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

class StaticPipeline(config: Config) extends Pipeline(config) {
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

  override val retirementStage: Stage = writeback

  override def connectStages() {
    // Debugging signals to ensure that PC and IR are passed through the whole
    // pipeline.
    val retiredPc = UInt(config.xlen bits)
    retiredPc := stages.last.output(data.PC)
    val retiredIr = UInt(config.xlen bits)
    retiredIr := stages.last.output(data.IR)

    val pipelineDataInfoMap =
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

      val firstStageId = Math.min(info.firstInputStageId, info.firstOutputStageId)
      val lastStageId = Math.max(info.lastInputStageId, info.lastOutputStageId)

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
  }
}
