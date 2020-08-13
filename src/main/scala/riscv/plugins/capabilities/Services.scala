package riscv.plugins.capabilities

import riscv._

trait ScrService {
  def getPcc(stage: Stage): Capability
  def getDdc(stage: Stage): Capability
}
