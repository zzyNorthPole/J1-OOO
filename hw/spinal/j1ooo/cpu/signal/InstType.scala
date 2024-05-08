package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object InstType extends SpinalEnum {
  val ALU = newElement()
  val JU = newElement()
  val TU = newElement()
  val MDU = newElement()
  val MFC0 = newElement()
  val MTC0 = newElement()
  val LOAD = newElement()
  val STORE = newElement()
  val CACHED = newElement()
  val CACHEI = newElement()
  val TLB = newElement()
  val ERET = newElement()
  val OTHERS = newElement()
}
