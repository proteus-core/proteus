package riscv.plugins.capabilities

import spinal.core._

object Opcodes {
  val CGetPerm        = M"111111100000-----000-----1011011"
  val CGetBase        = M"111111100010-----000-----1011011"
  val CGetLen         = M"111111100011-----000-----1011011"
  val CGetTag         = M"111111100100-----000-----1011011"
  val CGetOffset      = M"111111100110-----000-----1011011"
  val CGetAddr        = M"111111101111-----000-----1011011"

  val CSetBounds      = M"0001000----------000-----1011011"
  val CSetBoundsExact = M"0001001----------000-----1011011"
}
