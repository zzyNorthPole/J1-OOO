package j1ooo.cpu.execute.memory

import j1ooo.cpu.decode.AguCtrlBus
import j1ooo.cpu.signal.LsuOp
import j1ooo.gen.Config
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class AguBus() extends Bundle with IMasterSlave {
  val ctrl = AguCtrlBus()
  val din = Vec(UInt(32 bits), 2)

  override def asMaster(): Unit = {
    out(ctrl, din)
  }
}

class Agu extends Component {
  val io = new Bundle {
    val input = slave(AguBus())
    val addr = out UInt(32 bits)
    val addressErrorLoad = out Bool()
    val addressErroeStore = out Bool()
  }
  noIoPrefix()

  import io._

  addr := input.din(0) + input.din(1)
  addressErrorLoad := input.ctrl.valid && !input.ctrl.isWrite && (
    ((input.ctrl.op === LsuOp.H || input.ctrl.op === LsuOp.HU) && addr(0)) ||
      (input.ctrl.op === LsuOp.W && addr(0, 2 bits) =/= U(0, 2 bits))
  )
  addressErroeStore := input.ctrl.valid && input.ctrl.isWrite && (
    (input.ctrl.op === LsuOp.H && addr(0)) ||
      (input.ctrl.op === LsuOp.W && addr(0, 2 bits) =/= U(0, 2 bits))
  )
}