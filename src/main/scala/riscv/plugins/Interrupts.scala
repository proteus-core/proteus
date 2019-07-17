package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

private class Mip(implicit config: Config) extends Csr {
  val usip = False
  val ssip = False
  val msip = False
  val utip = False
  val stip = False
  val mtip = Reg(Bool()).init(False)
  val ueip = False
  val seip = False
  val meip = False

  val mip = B(0, config.xlen - 12 bits) ## meip ## False ## seip ## ueip ##
            mtip ## False ## stip ## utip ## msip ## False ## ssip ## usip

  override def read(): UInt = {
    mip.asUInt
  }

  override def write(value: UInt): Unit = {
    mtip := value(7)
  }

  // SW writes to mtip are not allowed
  override def swWrite(value: UInt): Unit = ()
}

private class Mie(implicit config: Config) extends Csr {
  val usie = False
  val ssie = False
  val msie = False
  val utie = False
  val stie = False
  val mtie = Reg(Bool()).init(False)
  val ueie = False
  val seie = False
  val meie = False

  val mie = B(0, config.xlen - 12 bits) ## meie ## False ## seie ## ueie ##
            mtie ## False ## stie ## utie ## msie ## False ## ssie ## usie

  override def read(): UInt = {
    mie.asUInt
  }

  override def write(value: UInt): Unit = {
    mtie := value(7)
  }
}

class Interrupts(implicit config: Config) extends Plugin with InterruptService {
  private var mtimer: MachineTimerIo = null

  override def getMachineTimerIo: MachineTimerIo = {
    assert(mtimer != null)
    mtimer
  }

  override def setup(pipeline: Pipeline): Unit = {
    val csr = pipeline.getService[CsrService]

    csr.registerCsr(pipeline, 0x304, new Mie)
    csr.registerCsr(pipeline, 0x344, new Mip)
  }

  override def build(pipeline: Pipeline): Unit = {
    val interruptStage = pipeline.writeback

    val interruptArea = interruptStage plug new Area {
      val trapHandler = pipeline.getService[TrapService]

      val mtimer = slave(new MachineTimerIo)
      val mstatus = slave(new CsrIo)
      val mie = slave(new CsrIo)
      val mip = slave(new CsrIo)

      when (mtimer.update) {
        val mipValue = UInt(config.xlen bits)
        mipValue := mip.read()
        mipValue(7) := mtimer.interruptPending // mtip bit
        mip.write(mipValue)
      }

      val gie = mstatus.read()(3) // mie bit
      val mtie = mie.read()(7)
      val mtip = mip.read()(7)

      when (gie) {
        when (mtie && mtip) {
          trapHandler.trap(pipeline, interruptStage,
                           TrapCause.MachineTimerInterrupt)
        }
      }
    }

    val pipelineArea = pipeline plug new Area {
      val mtimer = slave(new MachineTimerIo)
      mtimer <> interruptArea.mtimer

      val csr = pipeline.getService[CsrService]
      interruptArea.mstatus <> csr.getCsr(pipeline, 0x300)
      interruptArea.mie <> csr.getCsr(pipeline, 0x304)
      interruptArea.mip <> csr.getCsr(pipeline, 0x344)
    }

    pipelineArea.setName("")
    mtimer = pipelineArea.mtimer
  }
}
