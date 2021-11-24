package riscv

import riscv.plugins.scheduling.dynamic.ReorderBuffer
import spinal.core._

import scala.reflect.ClassTag

trait DynamicPipeline extends Pipeline {
  val issuePipeline: StaticPipeline
  val rsStages: Seq[Stage]
  var rob: ReorderBuffer = null
  val loadStage: Stage

  val unorderedStages: Seq[Stage]

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
    pipelineRegs = rsStages.map(stage => {
      val regs = new PipelineRegs(stage)
      regs.setName(s"pipelineRegs_${stage.stageName}")
      stage -> regs
    }).toMap
  }

  override def connectStages(): Unit = {

    for (stage <- unorderedStages :+ retirementStage) {
      stage.output(data.PC)
      stage.output(data.IR)

      // HACK!
      stage.output(data.RD)
      stage.output(data.RD_DATA_VALID)
      stage.output(data.RD_TYPE)
      stage.output(data.NEXT_PC)
      stage.value(data.RS1)
      stage.value(data.RS2)
      stage.value(data.RS1_TYPE)
      stage.value(data.RS2_TYPE)
    }

    // HACK make sure that all pipeline regs are routed through *all* exe stages.
    // I'm embarrassed...
    // https://gitlab.com/ProteusCore/ProteusCore/-/issues/17
    for (stage <- rsStages) {
      for (pipelineData <- stage.lastValues.keys) {
        for (stage <- rsStages) {
          stage.value(pipelineData)
        }
      }
    }

    val issueStage = issuePipeline.stages.last

    for (stage <- unorderedStages :+ retirementStage) {
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

    for (stage <- rsStages) {
      for (reg <- retirementStage.inputs.keys.toSet union loadStage.inputs.keys.toSet) {
        stage.output(reg)
      }
    }

    for (stage <- rsStages) {
      for ((inputData, input) <- stage.inputs) {
        val (regInput, regOutput) = pipelineRegs(stage).addReg(inputData)
        input := regOutput
        regInput := issueStage.output(inputData)
      }
    }

    // FIXME copy-pasted from StaticPipeline
    for (stage <- rsStages) {
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
