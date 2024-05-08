package j1ooo.cpu.util

import j1ooo.gen.Config
import spinal.core._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.{IMasterSlave, slave}

case class MulInputPayload(width: Int) extends Bundle {
  val signed = in Bool()
  val a = UInt(width bits)
  val b = UInt(width bits)
}

case class MulOutputPayload(width: Int) extends Bundle {
  val p = UInt((width * 2) bits)
}

case class MulBus(width: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val input = MulInputPayload(width)
  val output = MulOutputPayload(width)

  override def asMaster(): Unit = {
    out(valid, input)
    in(ready, output)
  }
}

class Mul(width: Int) extends Component {
  assert(width % 2 == 0)
  val io = new Bundle {
    val bus = slave(MulBus(width))
    val flush = in Bool()
  }
  noIoPrefix()

  import io._

  val signA = bus.input.a(width - 1)
  val signB = bus.input.b(width - 1)
  val absA = bus.input.signed ? bus.input.a.asSInt.abs | bus.input.a
  val absB = bus.input.signed ? bus.input.b.asSInt.abs | bus.input.b

  val pass = Bool()
  val hasStarted = RegInit(False)
  val part = Vec(UInt(width bits), 4)
  for (i <- 0 to 1) {
    for (j <- 0 to 1) {
      part(i * 2 + j) := (
        absA(((i + 1) * (width / 2) - 1) downto (i * (width / 2))) *
          absB(((j + 1) * (width / 2) - 1) downto (j * (width / 2)))
      )
    }
  }

  val partReg = Vec(RegInit(U(0, width bits)), 4)
  for (i <- 0 until 4) {
    partReg(i) := part(i)
  }

  val absP = UInt((width * 2) bits)
  absP := (
    (U(0, width bits) @@ partReg(0)) +
      (U(0, (width / 2) bits) @@ partReg(1) @@ U(0, (width / 2) bits)) +
      (U(0, (width / 2) bits) @@ partReg(2) @@ U(0, (width / 2) bits)) +
      (partReg(3) @@ U(0, width bits))
  )

  val absPReg = RegInit(U(0, (width * 2) bits))
  absPReg := absP

  val mulFSM = new StateMachine {
    setEntry(stateBoot)
    disableAutoStart()

    val finishMul = new State()
    // val finishAdd = new State()

    stateBoot.whenIsActive {
      when(!pass) {
        hasStarted.set()
        goto(finishMul)
      }.otherwise {
        hasStarted.clear()
      }
    }

    finishMul.whenIsActive {
      when(flush) {
        hasStarted.clear()
        goto(stateBoot)
      }.otherwise {
        goto(stateBoot)
      }
    }
  }

  pass := flush || (
    mulFSM.isActive(mulFSM.stateBoot) && (hasStarted || !io.bus.valid)
  )

  io.bus.ready := !flush && io.bus.valid && pass

  val signP = signA ^ signB

  bus.output.p := (bus.input.signed & signP) ? (U(0, (width * 2) bits) - absP) | absP
}

object MulVerilog extends App {
  Config.spinal.generateVerilog(new Mul(32))
}