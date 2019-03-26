package riscv

import spinal.core._
import spinal.core.sim._

class Core(imemHexPath: String) extends Component {
  import riscv.plugins._

  val pipelinePlugins = if (true) Seq(new SimplePipelining, new DataHazardResolver) else Seq(new NoPipelining)
  val plugins = pipelinePlugins ++ Seq(
    new Fetcher(imemHexPath),
    new Decoder,
    new RegisterFile,
    new IntAlu,
    new Lsu
  )
  val config = new Config(BaseIsa.RV32I, plugins)
  val pipeline = new Pipeline(config)
}

object Core {
  def main(args: Array[String]) {
    SpinalVerilog(new Core(args(0)))
  }
}

object CoreSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new Core(args(0))).doSim {dut =>
      dut.clockDomain.forkStimulus(10)

      for (i <- 0 to 100) {
        dut.clockDomain.waitSampling()
      }
    }
  }
}
