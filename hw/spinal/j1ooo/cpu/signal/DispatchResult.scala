package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object DispatchResult extends SpinalEnum {
  val NORMAL0 = newElement()
  val NORMAL1 = newElement()
  val MULDIV = newElement()
  val MEMORY = newElement()
  val NONE = newElement()
}
