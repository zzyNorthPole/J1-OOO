package j1ooo.cpu.execute.muldiv

import j1ooo.cpu.signal.MduOp
import j1ooo.cpu.util.{Div, Mul}
import j1ooo.gen.Config
import spinal.core._
import spinal.lib.{IMasterSlave, MuxOH, slave}

case class MduBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val op = MduOp()
  val din = Vec(UInt(32 bits), 2)
  val dout = Vec(UInt(32 bits), 2)

  override def asMaster(): Unit = {
    out(valid)
    in(ready)
    out(op)
    out(din)
    in(dout)
  }
}

class Mdu extends Component {
  val io = new Bundle {
    val bus = slave(MduBus())
    val flush = in Bool()
  }
  noIoPrefix()

  import io._
  import MduOp._

  val mul = new Mul(32)
  mul.io.flush := flush
  mul.io.bus.valid := bus.valid && (bus.op === MUL || bus.op === MULT || bus.op === MULTU)
  mul.io.bus.input.signed := !(bus.op === MduOp.MULTU)
  mul.io.bus.input.a := bus.din(0)
  mul.io.bus.input.b := bus.din(1)

  val div = new Div(32)
  div.io.flush := flush
  div.io.bus.valid := bus.valid && (bus.op === DIV || bus.op === DIVU)
  div.io.bus.input.signed := (bus.op === MduOp.DIV)
  div.io.bus.input.a := bus.din(0)
  div.io.bus.input.b := bus.din(1)

  // dout0 hi
  bus.dout(0) := MuxOH(
    Vec(
      bus.op === MUL || bus.op === MULT || bus.op === MULTU,
      bus.op === DIV || bus.op === DIVU,
      bus.op === MTHI
    ),
    Vec(
      mul.io.bus.output.p(63 downto 32),
      div.io.bus.output.r,
      bus.din(0)
    )
  )

  // dout1 lo
  bus.dout(1) := MuxOH(
    Vec(
      bus.op === MUL || bus.op === MULT || bus.op === MULTU,
      bus.op === DIV || bus.op === DIVU,
      bus.op === MTLO
    ),
    Vec(
      mul.io.bus.output.p(31 downto 0),
      div.io.bus.output.q,
      bus.din(0)
    )
  )

  bus.ready := (mul.io.bus.valid && mul.io.bus.ready) ||
    (div.io.bus.valid && div.io.bus.ready) ||
    (bus.op === MFHI || bus.op === MTHI || bus.op === MFLO || bus.op === MTLO)
}

object MduVerilog extends App {
  Config.spinal.generateVerilog(new Mdu())
}