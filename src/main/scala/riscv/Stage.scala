package riscv

import spinal.core._

import scala.collection.mutable

class Arbitration extends Bundle {
  val isValid = in Bool()
  val isReady = out Bool()
}

class Stage(val stageName: String) extends Component {
  definitionName = s"Stage_$stageName"

  val arbitration = new Arbitration

  def PipelineRegMap() = mutable.Map[PipelineData[Data], Data]()
  val inputs = PipelineRegMap()
  val outputs = PipelineRegMap()
  val outputsDefaults = PipelineRegMap()
  val lastValues = PipelineRegMap()

  def input[T <: Data](reg: PipelineData[T]): T = {
    inputs.getOrElseUpdate(reg.asInstanceOf[PipelineData[Data]], rework {
      val input = in(reg.dataType())
      input.setName(s"in_${reg.name}")
      input
    }).asInstanceOf[T]
  }

  def output[T <: Data](reg: PipelineData[T]): T = {
    val regAsData = reg.asInstanceOf[PipelineData[Data]]
    outputs.getOrElseUpdate(regAsData, rework {
      val output = out(reg.dataType())
      output.setName(s"out_${reg.name}")

      val default = reg.dataType()
      default.setName(s"_out_default_${reg.name}")
      outputsDefaults(regAsData) = default
      output := default
      output.allowOverride

      output
    }).asInstanceOf[T]
  }

  def value[T <: Data](reg: PipelineData[T]): T = {
    lastValues.getOrElseUpdate(reg.asInstanceOf[PipelineData[Data]], rework {
      val lastValue = reg.dataType()
      lastValue.setName(s"value_${reg.name}")
      lastValue
    }).asInstanceOf[T]
  }

  def connectOutputDefaults(): Unit = rework {
    for ((data, default) <- outputsDefaults) {
      if (inputs.contains(data)) {
        val input = inputs(data)
        default := input
      }
    }
  }

  def connectLastValues(): Unit = rework {
    for ((data, lastValue) <- lastValues) {
      if (outputs.contains(data)) {
        lastValue := output(data)
      } else if (inputs.contains(data)) {
        lastValue := input(data)
      } else {
        assert(false, s"No value for ${data.name} in stage ${stageName}")
      }
    }
  }
}
