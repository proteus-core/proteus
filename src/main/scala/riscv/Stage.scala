package riscv

import spinal.core._

import scala.collection.mutable

class Arbitration extends Bundle {
  val isValid = in Bool ()
  val isStalled = in Bool ()
  val isReady = out Bool ()
  val isDone = out Bool ()
  val rs1Needed, rs2Needed = out Bool ()
  val jumpRequested = out Bool ()

  /** Is this stage currently idling?
    *
    * True when there either is no instruction in the current stage or it is stalled. When
    * implementing a multi-cycle stage operation, make sure to reset the internal state machine when
    * this signal is high.
    */
  def isIdle: Bool = !isValid || isStalled

  /** Is this stage currently processing an instruction?
    *
    * True when there is a valid instruction that is not stalled in the current stage. Use in stage
    * logic to know when to process an instruction.
    */
  def isRunning: Bool = !isIdle

  val isAvailable = out Bool ()
  isAvailable := !isValid || isDone

  isReady := True
  isReady.allowOverride
  rs1Needed := False
  rs2Needed := False
  jumpRequested := False

  isDone := isValid && isReady && !isStalled
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
    inputs
      .getOrElseUpdate(
        reg.asInstanceOf[PipelineData[Data]],
        rework {
          val input = in(reg.dataType())
          input.setName(s"in_${reg.name}")
          input
        }
      )
      .asInstanceOf[T]
  }

  def hasInput[T <: Data](reg: PipelineData[T]) =
    inputs.contains(reg.asInstanceOf[PipelineData[Data]])

  def output[T <: Data](reg: PipelineData[T]): T = {
    val regAsData = reg.asInstanceOf[PipelineData[Data]]

    outputs
      .getOrElseUpdate(
        regAsData,
        reworkOutsideConditionScope {
          val output = out(reg.dataType())
          output.setName(s"out_${reg.name}")

          val default = reg.dataType()
          default.setName(s"_out_default_${reg.name}")
          default.assignDontCare()
          default.allowOverride
          outputsDefaults(regAsData) = default
          output := default
          output.allowOverride

          output
        }
      )
      .asInstanceOf[T]
  }

  def value[T <: Data](reg: PipelineData[T]): T = {
    lastValues
      .getOrElseUpdate(
        reg.asInstanceOf[PipelineData[Data]],
        rework {
          val lastValue = reg.dataType()
          lastValue.setName(s"value_${reg.name}")
          lastValue
        }
      )
      .asInstanceOf[T]
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

  /** Execute rtl outside any condition scopes. Used to unconditionally create the default output
    * connection even when output() is called inside a conditional statement.
    */
  private def reworkOutsideConditionScope[T](rtl: => T) = {
    val body = Component.current.dslBody
    body.push()
    val swapContext = body.swap()
    val ret = rework(rtl)
    body.pop()
    swapContext.appendBack()
    ret
  }
}
