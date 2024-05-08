package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object TuOp extends SpinalEnum {
  val TEQ = newElement()
  val TNE = newElement()
  val TGE = newElement()
  val TGEU = newElement()
  val TLT = newElement()
  val TLTU = newElement()
}
