package j1ooo.cpu.signal

import spinal.core.{SpinalEnum, SpinalEnumEncoding}

object Exception extends SpinalEnum {
  val INT = newElement() // Interrupt
  val MOD = newElement() // TLB modification exception
  val TLBL = newElement() // TLB exception(load or instruction fetch)
  val TLBS = newElement() // TLB exception(store)
  val ADEL = newElement() // Address error exception(load or instruction fetch)
  val ADES = newElement() // Address error exception(store)
  val SYS = newElement() // Syscall exception
  val BP = newElement() // Breakpoint exception
  val RI = newElement() // Reserved instruction exception
  val CPU = newElement() // Coprocessor Unusable exception
  val OV = newElement() // Arithmetic Overflow exception
  val TR = newElement() // Trap exception
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    INT -> 0,
    MOD -> 1,
    TLBL -> 2,
    TLBS -> 3,
    ADEL -> 4,
    ADES -> 5,
    SYS -> 8,
    BP -> 9,
    RI -> 10,
    CPU -> 11,
    OV -> 12,
    TR -> 13
  )
}
