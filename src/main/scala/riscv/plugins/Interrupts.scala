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
  val meip = Reg(Bool()).init(False)

  val mip = B(0, config.xlen - 12 bits) ## meip ## False ## seip ## ueip ##
            mtip ## False ## stip ## utip ## msip ## False ## ssip ## usip

  override def read(): UInt = {
    mip.asUInt
  }

  override def write(value: UInt): Unit = {
    mtip := value(7)
    meip := value(11)
  }

  // SW writes to mtip/meip are not allowed
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
  val meie = Reg(Bool()).init(False)

  val mie = B(0, config.xlen - 12 bits) ## meie ## False ## seie ## ueie ##
            mtie ## False ## stie ## utie ## msie ## False ## ssie ## usie

  override def read(): UInt = {
    mie.asUInt
  }

  override def write(value: UInt): Unit = {
    mtie := value(7)
    meie := value(11)
  }
}

class Interrupts(interruptStage: Stage) extends Plugin[Pipeline] with InterruptService {
  private var mtimer: IrqIo = null
  private var external: IrqIo = null

  override def getMachineTimerIrqIo: IrqIo = {
    assert(mtimer != null)
    mtimer
  }

  override def getExternalIrqIo: IrqIo = {
    assert(external != null)
    external
  }

  override def setup(): Unit = {
    val csr = pipeline.service[CsrService]

    csr.registerCsr(0x304, new Mie)
    csr.registerCsr(0x344, new Mip)
  }

  override def build(): Unit = {
    val interruptArea = interruptStage plug new Area {
      val trapHandler = pipeline.service[TrapService]

      val mtimer = slave(new IrqIo)
      val external = slave(new IrqIo)
      val mstatus = slave(new CsrIo)
      val mie = slave(new CsrIo)
      val mip = slave(new CsrIo)

      when (mtimer.update || external.update) {
        val mipValue = UInt(config.xlen bits)
        mipValue := mip.read()

        when (mtimer.update) {
          mipValue(7) := mtimer.interruptPending // mtip bit
        }

        when (external.update) {
          mipValue(11) := external.interruptPending // meip bit
        }

        mip.write(mipValue)
      }

      val gie = mstatus.read()(3) // mie bit
      val mtie = mie.read()(7)
      val mtip = mip.read()(7)
      val meie = mie.read()(11)
      val meip = mip.read()(11)

      when (gie) {
        when (mtie && mtip) {
          trapHandler.trap(interruptStage, TrapCause.MachineTimerInterrupt)
        }

        when (meie && meip) {
          trapHandler.trap(interruptStage, TrapCause.MachineExternalInterrupt)
        }
      }
    }

    val pipelineArea = pipeline plug new Area {
      val mtimer = slave(new IrqIo)
      mtimer <> interruptArea.mtimer
      val external = slave(new IrqIo)
      external <> interruptArea.external

      val csr = pipeline.service[CsrService]
      interruptArea.mstatus <> csr.getCsr(0x300)
      interruptArea.mie <> csr.getCsr(0x304)
      interruptArea.mip <> csr.getCsr(0x344)
    }

    pipelineArea.setName("")
    mtimer = pipelineArea.mtimer
    external = pipelineArea.external
  }
}
