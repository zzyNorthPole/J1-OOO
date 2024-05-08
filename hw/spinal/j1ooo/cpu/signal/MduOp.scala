package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object MduOp extends SpinalEnum {
  val MUL = newElement()
  val MULT = newElement()
  val MULTU = newElement()
  val DIV = newElement()
  val DIVU = newElement()
  val MFHI = newElement()
  val MTHI = newElement()
  val MFLO = newElement()
  val MTLO = newElement()
}
