package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object TlbOp extends SpinalEnum {
  val TLBP = newElement()
  val TLBR = newElement()
  val TLBWI = newElement()
  val TLBWR = newElement()
}
