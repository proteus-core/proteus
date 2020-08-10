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

  val CSpecialRW      = M"0000001----------000-----1011011"
}

object ScrIndex {
  val PCC = 0
  val DDC = 1

  val UTCC = 4
  val UTDC = 5
  val UScratchC = 6
  val UEPCC = 7

  val STCC = 12
  val STDC = 13
  val SScratchC = 14
  val SEPCC = 15

  val MTCC = 28
  val MTDC = 29
  val MScratchC = 30
  val MEPCC = 31
}
