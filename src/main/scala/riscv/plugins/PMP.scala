package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

//PMP implementation for Xlen = 32, hardware mode only, up to 32 bit addresses.
class PMP extends Plugin[Pipeline] with PMPService {

  private val CSR_PMPCSR0 = 0x3a0
  private val CSR_PMPADDR0 = 0x3b0

  private var csrService: CsrService = _

  private val pmpEntries = 16
  private val pmpCfgPerReg = 4
  private val pmpCsrCount = pmpEntries / pmpCfgPerReg

  private class csrPmpCfg(implicit config: Config) extends Csr {
    // TODO: Expand to also include 64-bit arch
    // 32-bit version stores 4 configs per register
    val csrBytes = Reg(UInt(config.isa.xlen bits)).init(0)

    override def read(): UInt = csrBytes

    def legalizePmpCsr(value: UInt)(implicit config: Config): UInt = {
      val R = value(0)
      val W = value(1)
      val X = value(2)
      val A = value(4 downto 3)
      val L = value(7)

      // Reserved combination: R=0 and W=1 → force W=0
      val W_fixed = (W & R)

      // Reserved bits [6:5] are forced to zero
      val result = UInt(8 bits)
      result := 0
      result(7) := L
      result(4 downto 3) := A
      result(2) := X
      result(1) := W_fixed
      result(0) := R

      return result
    }

    override def write(value: UInt): Unit = {
      for (i <- 0 until pmpCfgPerReg) {
        val bytes = value((i * 8 + 7) downto (i * 8))
        val locked = csrBytes(i * 8 + 7)
        when(!locked) {
          csrBytes((i * 8 + 7) downto (i * 8)) := legalizePmpCsr(bytes)
        }
      }
    }
  }

  private class csrPmpAddr(num: Int)(implicit config: Config) extends Csr {
    val addr = Reg(UInt(config.isa.xlen bits)).init(0)
    override def read(): UInt = addr

    val cfgReg = num / pmpCfgPerReg
    val cfgPos = num % pmpCfgPerReg
    val cfgRegNext = (num + 1) / pmpCfgPerReg
    val cfgPosNext = (num + 1) % pmpCfgPerReg

    override def write(addr: UInt): Unit = {
      val stub_CfgSelf = slave(new CsrIo)
      pipeline plug {
        stub_CfgSelf <> csrService.getCsr(CSR_PMPCSR0 + cfgReg)
      }
      val cfgSelf = stub_CfgSelf.read()
      val lockedBySelf = Bool()
      lockedBySelf := cfgSelf(7 + 8 * cfgPos)
      val lockedByNext = Bool()
      lockedByNext := False
      if (num != pmpEntries - 1) { // reg needs to be locked when the next entry is locked and in TOR mode
        val stub_CfgNext = slave(new CsrIo)
        pipeline plug {
          stub_CfgNext <> csrService.getCsr(CSR_PMPCSR0 + cfgRegNext)
        }
        val cfgNext = stub_CfgNext.read()(7 + 8 * cfgPosNext downto 8 * cfgPosNext)
        val mode = cfgNext(4 downto 3)
        when(mode === 1) {
          lockedByNext := cfgNext(7)
        }
      }
      when(!(lockedBySelf | lockedByNext)) {
        this.addr(29 downto 0) := addr(29 downto 0)
      }
    }
  }

  override def setup(): Unit = {
    pipeline plug new Area {
      csrService = pipeline.service[CsrService]
      for (i <- 0 until pmpCsrCount) {
        csrService.registerCsr(CSR_PMPCSR0 + i, new csrPmpCfg)
      }
      for (j <- 0 until pmpEntries) {
        csrService.registerCsr(CSR_PMPADDR0 + j, new csrPmpAddr(j))
      }
    }
  }

  override def isAllowedToRead(addr: UInt): Bool = {
    isAllowed(addr, 0)
  }

  override def isAllowedToWrite(addr: UInt): Bool = {
    isAllowed(addr, 1)
  }

  override def isAllowedToExecute(addr: UInt): Bool = {
    isAllowed(addr, 2)
  }

  private def isAllowed(addr: UInt, perm: UInt): Bool = {
    case class PermData()(implicit config: Config) extends Bundle {
      val allow = Bool()
      val matched = Bool()
    }

    def updatePermission(allow: Bool, data: PermData) {
      data.allow := allow
      data.matched := True
    }

    val permData = PermData()
    permData.allow := True
    permData.matched := False

    for (i <- (0 until pmpCsrCount).reverse) {
      val stub_fullCfg = slave(new CsrIo)
      pipeline plug {
        stub_fullCfg <> csrService.getCsr(CSR_PMPCSR0 + i)
      }
      val fullCfg = stub_fullCfg.read()

      for (j <- (0 until pmpCfgPerReg).reverse) {
        val cfg = fullCfg(7 + 8 * j downto 8 * j)
        val stub_pmpaddr = slave(new CsrIo)
        pipeline plug {
          stub_pmpaddr <> csrService.getCsr(CSR_PMPADDR0 + i * 4 + j)
        }
        val pmpaddr = stub_pmpaddr.read()

        val a = cfg(4 downto 3)
        val l = cfg(7)

        when(a === 1) { // TOR
          val bottom = UInt(config.isa.xlen bits)
          if (i == 0 && j == 0) {
            bottom := 0
          } else {
            val stub_bottom = slave(new CsrIo)
            pipeline plug {
              stub_bottom <> csrService.getCsr(CSR_PMPADDR0 + i * 4 + j - 1)
            }
            bottom := stub_bottom.read()
          }
          val bot = bottom << 2
          val top = pmpaddr << 2
          when(addr >= bot && addr < top) {
            updatePermission(!l | cfg(perm), permData)
          }
        }
        when(a === 2) { // NA4
          val base = pmpaddr << 2
          when(addr >= base && addr < base + 4) {
            updatePermission(!l | cfg(perm), permData)
          }
        }
        when(a === 3) { // NAPOT
          val size =
            (~pmpaddr & (pmpaddr + 1)) << 3 // Could fail when pmpaddr is all ones. Will have a one at the position of the rightmost 0 (shifted over by 3)
          val top = (pmpaddr + 1) << 2

          when(addr >= top - size && addr < (top)) {
            updatePermission(!l | cfg(perm), permData)
          }
        }
      }
    }

    permData.allow
  }
}
