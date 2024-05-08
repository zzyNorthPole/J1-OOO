package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object CacheOp extends SpinalEnum {
  val indexInvalidateWriteBack = newElement()
  val hitInvalidateNotWriteBack = newElement()
  val hitInvalidateWriteBack = newElement()
  val cacheOpMiss = newElement()
}
