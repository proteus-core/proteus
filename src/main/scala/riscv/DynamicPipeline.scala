package riscv

import spinal.core._

import scala.reflect.ClassTag

trait DynamicPipeline extends Pipeline {
  val issuePipeline: StaticPipeline
  val exeStages: Seq[Stage]

  var pipelineRegs: Map[Stage, PipelineRegs] = null


  override def fetchStage: Stage = null
  override val retirementStage: Stage = null

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
  }

  override def connectStages(): Unit = {
    pipelineRegs = exeStages.map(stage => {
      val regs = new PipelineRegs(stage)
      regs.setName(s"pipelineRegs_${stage.stageName}")
      stage -> regs
    }).toMap

    for (stage <- exeStages) {
      stage.input(data.PC)
      stage.input(data.IR)
      stage.output(data.PC)
      stage.output(data.IR)
    }

    val issueStage = issuePipeline.stages.last

    for (stage <- exeStages) {
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
