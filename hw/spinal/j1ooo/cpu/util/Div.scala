package j1ooo.cpu.util

import j1ooo.gen.Config
import spinal.core._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.{Counter, IMasterSlave, master, slave}

case class DivInputPayload(width: Int) extends Bundle {
  // a / b
  val signed = in Bool()
  val a = UInt(width bits)
  val b = UInt(width bits)
}

case class DivOutputPayload(width: Int) extends Bundle {
  // a = b * q + r
  val q = UInt(width bits)
  val r = UInt(width bits)
}

case class DivBus(width: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val input = DivInputPayload(width)
  val output = DivOutputPayload(width)

  override def asMaster(): Unit = {
    out(valid, input)
    in(ready, output)
  }
}

class Div(width: Int) extends Component {
  val io = new Bundle {
    val bus = slave(DivBus(width))
    val flush = in Bool()
  }
  noIoPrefix()

  import io._

  val signA = bus.input.a(width - 1)
  val signB = bus.input.b(width - 1)
  val absA = bus.input.signed ? bus.input.a.asSInt.abs | bus.input.a
  val absB = bus.input.signed ? bus.input.b.asSInt.abs | bus.input.b

  // pass control the enter of the FSM
  val pass = Bool()
  // hasStarted control the FSM
  val hasStarted = RegInit(False)
  // count mark the end of the div op
  val count = Counter(0 until width)
  // preA + sufA just like a pointer of A
  val preA = RegInit(U(0, (width + 1) bits))
  val sufA = RegInit(U(0, width bits))
  val curPreA, nextPreA = UInt((width + 1) bits)
  val nextSufA = UInt(width bits)
  curPreA := preA((width - 1) downto 0) @@ sufA((width - 1) downto (width - 1))
  nextSufA := sufA((width - 2) downto 0) @@ U(0, 1 bits)
  val tryDiv = UInt((width + 1) bits)
  tryDiv := curPreA - (U(0, 1 bits) @@ absB)
  nextPreA := tryDiv(width) ? curPreA | tryDiv
  val absQ, absR = RegInit(U(0, width bits))
  val divFSM = new StateMachine {
    setEntry(stateBoot)
    disableAutoStart()

    val execute = new State()

    stateBoot.whenIsActive {
      when(!pass) {
        hasStarted.set()
        count.clear()
        preA := 0
        sufA := absA
        absQ := 0
        absR := 0
        goto(execute)
      }.otherwise {
        hasStarted.clear()
      }
    }

    execute.whenIsActive {
      when(flush) {
        hasStarted.clear()
        count.clear()
        goto(stateBoot)
      }.otherwise {
        count.increment()
        preA := nextPreA
        sufA := nextSufA
        absQ(width - 1 - count.asSInt.asUInt) := !tryDiv(width)
        absR := nextPreA((width - 1) downto 0)
        when(count.willOverflow) {
          goto(stateBoot)
        }
      }
    }
  }

  // when flush terminate immediately
  pass := flush || (
    divFSM.isActive(divFSM.stateBoot) && (hasStarted || !io.bus.valid)
  )

  io.bus.ready := !flush && io.bus.valid && pass

  val signQ = signA ^ signB
  val signR = signA
  bus.output.q := (bus.input.signed & signQ) ? (U(0, width bits) - absQ) | absQ
  bus.output.r := (bus.input.signed & signR) ? (U(0, width bits) - absR) | absR
}

object DivVerilog extends App {
  Config.spinal.generateVerilog(new Div(32))
}