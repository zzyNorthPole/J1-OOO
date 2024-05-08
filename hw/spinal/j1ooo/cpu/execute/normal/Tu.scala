package j1ooo.cpu.execute.normal

import j1ooo.cpu.signal.TuOp
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class TuBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val op = TuOp()
  val din = Vec(UInt(32 bits), 2)

  override def asMaster(): Unit = {
    out(valid, op, din)
  }
}
class Tu extends Component {
  val io = new Bundle {
    val bus = slave(TuBus())
    val trap = out Bool()
  }
  noIoPrefix()

  import io._
  import TuOp._

  trap := bus.valid && bus.op.mux(
    TEQ -> (bus.din(0) === bus.din(1)),
    TNE -> (bus.din(0) =/= bus.din(1)),
    TGE -> (bus.din(0).asSInt >= bus.din(1).asSInt),
    TGEU -> (bus.din(0) >= bus.din(1)),
    TLT -> (bus.din(0).asSInt < bus.din(1).asSInt),
    TLTU -> (bus.din(0) < bus.din(1))
  )
}
