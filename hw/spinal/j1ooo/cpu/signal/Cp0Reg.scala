package j1ooo.cpu.signal

import spinal.core.{SpinalEnum, SpinalEnumEncoding}

object Cp0Reg extends SpinalEnum {
  val Index = newElement()
  val Random = newElement()
  val EntryLo0 = newElement()
  val EntryLo1 = newElement()
  val Context = newElement()
  val PageMask = newElement()
  val Wired = newElement()
  val BadVAddr = newElement()
  val Count = newElement()
  val EntryHi = newElement()
  val Compare = newElement()
  val Status = newElement()
  val Cause = newElement()
  val EPC = newElement()
  val PRIdEBase = newElement()
  val Config = newElement()
  val ErrorEPC = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    Index -> 0,
    Random -> 1,
    EntryLo0 -> 2,
    EntryLo1 -> 3,
    Context -> 4,
    PageMask -> 5,
    Wired -> 6,
    BadVAddr -> 8,
    Count -> 9,
    EntryHi -> 10,
    Compare -> 11,
    Status -> 12,
    Cause -> 13,
    EPC -> 14,
    PRIdEBase -> 15,
    Config -> 16,
    ErrorEPC -> 30
  )
}
