package j1ooo.cpu.execute.muldiv

import j1ooo.cpu.signal.MduOp
import j1ooo.gen.Config
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class HiLoReg() extends Bundle {
  val Hi, Lo = UInt(32 bits)

  def init() = {
    Hi init 0
    Lo init 0
    this
  }
}

case class HiLoReadBus() extends Bundle with IMasterSlave {
  val op = MduOp()
  val dout = UInt(32 bits)

  override def asMaster(): Unit = {
    out(op)
    in(dout)
  }
}

case class HiLoWriteBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val op = MduOp()
  val din = Vec(UInt(32 bits), 2)

  override def asMaster(): Unit = {
    out(valid, op, din)
  }

  def << (that: HiLoWriteBus): Unit = {
    this.valid := that.valid
    this.op := that.op
    this.din := that.din
  }
}

class HiLo extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val rPorts = slave(HiLoReadBus())
    val wPorts = slave(HiLoWriteBus())
    val commitPorts = Vec(slave(HiLoWriteBus()), 2)
  }
  noIoPrefix()

  import io._
  import MduOp._

  val AHL, PHL = Reg(HiLoReg()).init()
  when(commitPorts(0).valid || commitPorts(1).valid) {
    when(commitPorts(1).valid) {
      when(commitPorts(1).op === MULT || commitPorts(1).op === MULTU || commitPorts(1).op === MTHI || commitPorts(1).op === DIV || commitPorts(1).op === DIVU) {
        AHL.Hi := commitPorts(1).din(0)
      }
      when(commitPorts(1).op === MULT || commitPorts(1).op === MULTU || commitPorts(1).op === MTLO || commitPorts(1).op === DIV || commitPorts(1).op === DIVU) {
        AHL.Lo := commitPorts(1).din(1)
      }
    }.otherwise {
      when(commitPorts(0).op === MULT || commitPorts(0).op === MULTU || commitPorts(0).op === MTHI || commitPorts(0).op === DIV || commitPorts(0).op === DIVU) {
        AHL.Hi := commitPorts(0).din(0)
      }
      when(commitPorts(0).op === MULT || commitPorts(0).op === MULTU || commitPorts(0).op === MTLO || commitPorts(0).op === DIV || commitPorts(0).op === DIVU) {
        AHL.Lo := commitPorts(0).din(1)
      }
    }
  }

  when(flush) {
    when(
      commitPorts(0).valid &&
        (commitPorts(0).op === MULT || commitPorts(0).op === MULTU || commitPorts(0).op === MTHI || commitPorts(0).op === DIV || commitPorts(0).op === DIVU)
    ) {
      PHL.Hi := commitPorts(0).din(0)
    }.elsewhen(
      commitPorts(1).valid &&
        (commitPorts(1).op === MULT || commitPorts(1).op === MULTU || commitPorts(1).op === MTHI || commitPorts(1).op === DIV || commitPorts(1).op === DIVU)
    ) {
      PHL.Hi := commitPorts(1).din(0)
    }.otherwise {
      PHL.Hi := AHL.Hi
    }

    when(
      commitPorts(0).valid &&
        (commitPorts(0).op === MULT || commitPorts(0).op === MULTU || commitPorts(0).op === MTLO || commitPorts(0).op === DIV || commitPorts(0).op === DIVU)
    ) {
      PHL.Lo := commitPorts(0).din(1)
    }.elsewhen(
      commitPorts(1).valid &&
        (commitPorts(1).op === MULT || commitPorts(1).op === MULTU || commitPorts(1).op === MTLO || commitPorts(1).op === DIV || commitPorts(1).op === DIVU)
    ) {
      PHL.Lo := commitPorts(1).din(1)
    }.otherwise {
      PHL.Lo := AHL.Lo
    }
  }.otherwise {
    when(
      wPorts.valid &&
        (wPorts.op === MULT || wPorts.op === MULTU || wPorts.op === MTHI || wPorts.op === DIV || wPorts.op === DIVU)
    ) {
      PHL.Hi := wPorts.din(0)
    }

    when(
      wPorts.valid &&
        (wPorts.op === MULT || wPorts.op === MULTU || wPorts.op === MTLO || wPorts.op === DIV || wPorts.op === DIVU)
    ) {
      PHL.Lo := wPorts.din(1)
    }
  }

  rPorts.dout := rPorts.op.mux(
    MFHI -> PHL.Hi,
    MFLO -> PHL.Lo,
    default -> U(0, 32 bits)
  )
}

object HiLoVerilog extends App {
  Config.spinal.generateVerilog(new HiLo())
}