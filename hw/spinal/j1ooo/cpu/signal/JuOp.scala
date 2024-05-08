package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object JuOp extends SpinalEnum {
  val BLTZ = newElement()
  val BGEZ = newElement()
  val BLTZAL = newElement()
  val BGEZAL = newElement()
  val BEQ = newElement()
  val BNE = newElement()
  val BLEZ = newElement()
  val BGTZ = newElement()
  val JR = newElement()
  val JALR = newElement()
  val J = newElement()
  val JAL = newElement()
}