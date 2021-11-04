package riscv

import riscv.plugins.scheduling.dynamic.ReorderBuffer
import spinal.core._

import scala.reflect.ClassTag

trait DynamicPipeline extends Pipeline {
  val issuePipeline: StaticPipeline
  val exeStages: Seq[Stage]
  var rob: ReorderBuffer = null
  val loadStage: Stage

  var pipelineRegs: Map[Stage, PipelineRegs] = null


  override def fetchStage: Stage = null

  override def build(): Unit = {
    issuePipeline.initBuild()
    initBuild()

    issuePipeline.setupPlugins()
    setupPlugins()

    issuePipeline.buildPlugins()
    buildPlugins()

    finishBuild()
    issuePipeline.finishBuild()
  }

  override def init(): Unit = {
    pipelineRegs = exeStages.map(stage => {
      val regs = new PipelineRegs(stage)
      regs.setName(s"pipelineRegs_${stage.stageName}")
      stage -> regs
    }).toMap
  }

  override def connectStages(): Unit = {

    for (stage <- exeStages :+ retirementStage :+ loadStage) {
      stage.output(data.PC)
      stage.output(data.IR)

      // HACK!
      stage.output(data.RD)
      stage.output(data.RD_VALID)
      stage.output(data.RD_TYPE)
      stage.output(data.NEXT_PC)
    }

    val issueStage = issuePipeline.stages.last

    for (stage <- exeStages :+ retirementStage :+ loadStage) {
      // FIXME copy-pasted from StaticPipeline
      for (valueData <- stage.lastValues.keys) {
        if (!stage.outputs.contains(valueData)) {
          stage.input(valueData)
        }
      }

      // Make sure we get the input from the issue stage for all requested outputs.
      for (outputData <- stage.outputs.keys) {
        stage.input(outputData)
      }
    }

    for (stage <- exeStages) {
      for (reg <- retirementStage.inputs.keys.toSet union loadStage.inputs.keys.toSet) {
        stage.output(reg)
      }
    }

    for (stage <- exeStages) {
      for ((inputData, input) <- stage.inputs) {
        val (regInput, regOutput) = pipelineRegs(stage).addReg(inputData)
        input := regOutput
        regInput := issueStage.output(inputData)
      }
    }

    // FIXME copy-pasted from StaticPipeline
    for (stage <- exeStages) {
      stage.connectOutputDefaults()
      stage.connectLastValues()
    }
  }

  override def getService[T](implicit tag: ClassTag[T]): T = {
    if (super.hasService[T]) {
      super.getService[T]
    } else {
      issuePipeline.getService[T]
    }
  }

  override def hasService[T](implicit tag: ClassTag[T]): Boolean = {
    super.hasService[T] || issuePipeline.hasService[T]
  }
}
