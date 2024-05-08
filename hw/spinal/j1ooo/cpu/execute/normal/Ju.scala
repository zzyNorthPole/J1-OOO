package j1ooo.cpu.execute.normal

import j1ooo.cpu.signal.JuOp
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class CheckResult() extends Bundle {
  val fail = Bool()
  val pc = UInt(32 bits)
}

case class JuBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val op = JuOp()
  val din = Vec(SInt(32 bits), 2)
  val dout = UInt(32 bits)
  val pc, offset = UInt(32 bits)
  val predictPc = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, op, din, pc, offset, predictPc)
    in(dout)
  }
}

class Ju extends Component {
  val io = new Bundle {
    val bus = slave(JuBus())
    val checkResult = out(CheckResult())
    val bpuResult = out(CheckResult())
  }
  noIoPrefix()

  import io._

  import JuOp._
  val isJump = bus.op.mux(
    BLTZ -> (bus.din(0) < 0),
    BGEZ -> (bus.din(0) >= 0),
    BLTZAL -> (bus.din(0) < 0),
    BGEZAL -> (bus.din(0) >= 0),
    BEQ -> (bus.din(0) === bus.din(1)),
    BNE -> (bus.din(0) =/= bus.din(1)),
    BLEZ -> (bus.din(0) <= 0),
    BGTZ -> (bus.din(0) > 0),
    default -> True
  )

  val jumpTarget = bus.op.mux(
    JR -> bus.din(0).asUInt,
    JALR -> bus.din(0).asUInt,
    J -> bus.offset,
    JAL -> bus.offset,
    default -> (bus.pc + 4 + bus.offset)
  )

  val notJumpTarget = bus.pc + 8

  // dout used for link
  bus.dout := notJumpTarget

  checkResult.pc := isJump ? jumpTarget | notJumpTarget
  checkResult.fail := bus.valid && (checkResult.pc =/= bus.predictPc)

  bpuResult.pc := jumpTarget
  bpuResult.fail := checkResult.fail
}
