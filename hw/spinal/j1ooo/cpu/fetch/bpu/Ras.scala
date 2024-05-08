package j1ooo.cpu.fetch.bpu

import j1ooo.gen.RasConfig
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class RasItem(rasConfig: RasConfig) extends Bundle {
  val stack = Vec(UInt(rasConfig.width bits), rasConfig.lines)
  val top = UInt(log2Up(rasConfig.lines) bits)

  def init() = {
    stack init 0
    top init 0
    this
  }
}

case class RasPushBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val target = UInt (32 bits)

  override def asMaster(): Unit = {
    out(valid, target)
  }
}

case class RasRefreshBus(rasConfig: RasConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val item = RasItem(rasConfig)

  override def asMaster(): Unit = {
    out(valid, item)
  }
}

class Ras(rasConfig: RasConfig) extends Component {
  val io = new Bundle {
    val queryPort = new Bundle {
      val pc = in UInt(32 bits)
      val target = out UInt(32 bits)
      val curRas = out(RasItem(rasConfig))
    }

    val pushPort = slave(RasPushBus())

    val popPort = new Bundle {
      val valid = in Bool()
    }

    val flushPort = slave(RasRefreshBus(rasConfig))
  }
  noIoPrefix()

  import io._

  val ras = Reg(RasItem(rasConfig))
  ras.init()

  queryPort.target := queryPort.pc(2 + rasConfig.width, 32 - 2 - rasConfig.width bits) @@ ras.stack(ras.top) @@ U(0, 2 bits)

  queryPort.curRas := ras

  val nextTop = UInt(log2Up(rasConfig.lines) bits)
  when(pushPort.valid) {
    nextTop := ras.top + 1
  }.elsewhen(popPort.valid) {
    nextTop := ras.top - 1
  }.otherwise {
    nextTop := ras.top
  }

  when(flushPort.valid) {
    ras := flushPort.item
  }.otherwise {
    when(pushPort.valid) {
      ras.stack(nextTop) := pushPort.target(2, rasConfig.width bits)
    }
    when(pushPort.valid | popPort.valid) {
      ras.top := nextTop
    }
  }
}
