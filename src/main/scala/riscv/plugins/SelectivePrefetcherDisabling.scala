package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class SelectivePrefetcherDisabling(prefetcherDisablingCsrId: Int = 0x70b)(implicit config: Config)
    extends Plugin[Pipeline]
    with SelectivePrefetcherDisablingService {
  private var prefetcherDisablingCsrIo: CsrIo = null

  private def convertToPrefetcherDisablingMode(
      value: UInt
  ): SpinalEnumCraft[PrefetcherDisablingMode.type] = {
    val mode = PrefetcherDisablingMode()
    switch(value) {
      is(0) {
        mode := PrefetcherDisablingMode.NONE
      }
      is(1) {
        mode := PrefetcherDisablingMode.DMP
      }
      default {
        mode := PrefetcherDisablingMode.ALL
      }
    }
    mode
  }

  private class prefetcherDisablingCsr(implicit config: Config) extends Csr {
    val prefetcherDisablingMode = RegInit(PrefetcherDisablingMode.NONE)
    override def read(): UInt = prefetcherDisablingMode.asBits.asUInt.resize(config.isa.xlen bits)
    override def write(mode: UInt): Unit = {
      prefetcherDisablingMode := convertToPrefetcherDisablingMode(mode)
    }
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      pipeline.service[CsrService].registerCsr(prefetcherDisablingCsrId, new prefetcherDisablingCsr)
    }
  }

  override def getPrefetcherDisablingMode(
      isStage: Boolean
  ): SpinalEnumCraft[PrefetcherDisablingMode.type] = {
    if (isStage) { // build a new CsrIo
      def readOnlyCsr(csrId: Int): CsrIo = {
        val csrIn = slave(new CsrIo)
        pipeline plug new Area {
          csrIn <> pipeline.service[CsrService].getCsr(csrId)
        }
        csrIn.wdata.assignDontCare()
        csrIn.write := False
        csrIn
      }
      val newPrefetcherDisablingCsrIo = readOnlyCsr(prefetcherDisablingCsrId)
      convertToPrefetcherDisablingMode(newPrefetcherDisablingCsrIo.read())
    } else {
      if (prefetcherDisablingCsrIo == null) {
        pipeline plug new Area {
          def readOnlyCsr(csrId: Int): CsrIo = {
            val csr = pipeline.service[CsrService].getCsr(csrId)
            csr.wdata.assignDontCare()
            csr.write := False
            csr
          }
          prefetcherDisablingCsrIo = readOnlyCsr(prefetcherDisablingCsrId)
        }
      }
      // use the prefetcherDisablingCsrIo plugged into `pipeline`
      convertToPrefetcherDisablingMode(prefetcherDisablingCsrIo.read())
    }
  }
}
