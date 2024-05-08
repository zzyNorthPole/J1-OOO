package j1ooo.cpu.signal

import spinal.core.SpinalEnum

object AluOp extends SpinalEnum {
  val ADD = newElement()
  val ADDU = newElement()
  val SUB = newElement()
  val SUBU = newElement()
  val AND = newElement()
  val OR = newElement()
  val XOR = newElement()
  val NOR = newElement()
  val SLT = newElement()
  val SLTU = newElement()
  val LUI = newElement()
  val SLL = newElement()
  val SRL = newElement()
  val SRA = newElement()
  val MOVN = newElement()
  val MOVZ = newElement()
  val CLO = newElement()
  val CLZ = newElement()
}

