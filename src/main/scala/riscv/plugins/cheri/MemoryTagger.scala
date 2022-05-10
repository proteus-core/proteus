package riscv.plugins.cheri

import riscv._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// noinspection ForwardReference
class MemoryTagger(memoryStart: BigInt, memorySize: BigInt)(implicit context: Context)
  extends Plugin[Pipeline] with TaggedMemoryService {
  assert(memorySize % (context.clen / 8) == 0)
  private val numTags = memorySize / (context.clen / 8)

  private var masterCapBus: CapBus = _
  private var slaveCapBus: CapBus = _

  override def build(): Unit = {
    pipeline.service[MemoryService].filterDBus { (stage, dbusIn, dbusOut) =>
      slaveCapBus = CapBus().setName("cbus")
      build(stage, dbusIn, slaveCapBus, dbusOut)
    }
  }

  def build(stage: Stage, dbusIn: MemBus, cbusIn: CapBus, dbusOut: MemBus): Unit = {
    pipeline plug new StateMachine {
      val tags = Mem(Seq.fill(numTags.toInt) {False})

      dbusIn.cmd.ready := False
      dbusIn.rsp.valid := False
      dbusIn.rsp.payload.rdata.assignDontCare()
      val dbusControl = new MemBusControl(dbusOut)

      cbusIn.cmd.ready := False
      cbusIn.rsp.valid := False
      cbusIn.rsp.payload.rdata.assignDontCare()

      val address = dbusIn.cmd.valid ? dbusIn.cmd.payload.address | cbusIn.cmd.payload.address
      val addressInMemory = (address >= memoryStart) && (address < memoryStart + memorySize)
      val tagIndex = ((address - memoryStart) >> log2Up(context.clen / 8)).resized

      val cbusPayload = cbusIn.cmd.payload
      val cbusWordCtr = Counter(context.clen / config.xlen)
      val cbusTag = cbusPayload.wdata.tag
      val cbusWdata = cbusPayload.wdata.value
      val cbusWords = cbusWdata.subdivideIn(config.xlen bits)
      val cbusWord = cbusWords(cbusWordCtr)
      val cbusWordAddress = cbusPayload.address + (cbusWordCtr << log2Up(config.xlen / 8))

      val cbusReadWords = Vec(Reg(UInt(config.xlen bits)), context.clen / config.xlen - 1)

      val PASS_THROUGH: State = new State with EntryPoint {
        whenIsActive {
          when (dbusIn.cmd.valid) {
            val payload = dbusIn.cmd.payload

            when (payload.write) {
              val accepted = dbusControl.write(payload.address, payload.wdata, payload.wmask)

              when (accepted) {
                dbusIn.cmd.ready := True

                when (addressInMemory) {
                  tags(tagIndex) := False
                }
              }
            } otherwise {
              val (valid, rdata) = dbusControl.read(payload.address)

              when (valid) {
                dbusIn.cmd.ready := True
                dbusIn.rsp.valid := True
                dbusIn.rsp.payload.rdata := rdata
              }
            }
          } elsewhen (cbusIn.cmd.valid) {
            when (cbusPayload.write) {
              val accepted = dbusControl.write(cbusWordAddress, cbusWord, B"1111")

              when (accepted) {
                cbusWordCtr.increment()
                goto(CAP_OP)
              }
            } otherwise {
              val (valid, rdata) = dbusControl.read(cbusWordAddress)

              when (valid) {
                cbusReadWords(cbusWordCtr) := rdata
                cbusWordCtr.increment()
                goto(CAP_OP)
              }
            }
          }
        }
      }

      val CAP_OP = new State {
        whenIsActive {
          when (cbusPayload.write) {
            val accepted = dbusControl.write(cbusWordAddress, cbusWord, B"1111")

            when (accepted) {
              when (cbusWordCtr.willOverflowIfInc) {
                when (addressInMemory) {
                  tags(tagIndex) := cbusTag
                }

                cbusIn.cmd.ready := True
                goto(PASS_THROUGH)
              }

              cbusWordCtr.increment()
            }
          } otherwise {
            val (valid, rdata) = dbusControl.read(cbusWordAddress)

            when (valid) {
              when (cbusWordCtr.willOverflowIfInc) {
                cbusIn.rsp.rdata.assignValue((rdata ## cbusReadWords).asUInt)
                cbusIn.rsp.rdata.tag := tags(tagIndex)
                cbusIn.cmd.ready := True
                cbusIn.rsp.valid := True
                goto(PASS_THROUGH)
              } otherwise {
                cbusReadWords(cbusWordCtr) := rdata
              }

              cbusWordCtr.increment()
            }
          }
        }
      }
    }
  }


  override def finish(): Unit = {
    pipeline plug {
      masterCapBus <> slaveCapBus
    }
  }

  override def createCapBus(stage: Stage): CapBus = {
    assert(masterCapBus == null)

    stage plug {
      masterCapBus = master(CapBus()).setName("cbus")
    }

    masterCapBus
  }
}
