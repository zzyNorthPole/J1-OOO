package j1ooo.cpu.execute.muldiv

import j1ooo.cpu.decode.{Cp0CtrlBus, MduCtrlBus}
import j1ooo.cpu.execute.normal.BypassInputBus
import j1ooo.cpu.signal.InstType
import j1ooo.gen.{Config, RSConfig, RobConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, MuxOH, master, slave, traversableOnceBoolPimped}

case class MuldivRsItem(robConfig: RobConfig) extends Bundle {
  val valid = Bool()
  val src = Vec(new Bundle {
    val valid = Bool()
    val addr = UInt(log2Up(robConfig.lines) bits)
    val wake = Bool()
    val available = Bool()
    val data = UInt(32 bits)
  }, 2)
  val dest = new Bundle {
    val valid = Bool()
    val addr = UInt(log2Up(robConfig.lines) bits)
  }
  val ctrl = new Bundle {
    val mdu = MduCtrlBus()
    val mtc0 = Cp0CtrlBus()
    val mfc0 = Cp0CtrlBus()
    val instType = InstType()
  }
  val pc = UInt(32 bits)
}

case class MuldivRsInputBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Vec(Bool(), 2)
  val ready = Vec(Bool(), 2)
  val payload = Vec(MuldivRsItem(robConfig), 2)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready)
  }

  def << (that: MuldivRsInputBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
    that.ready := this.ready
  }
}

case class MuldivRsOutputBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val payload = MuldivRsItem(robConfig)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready)
  }

  def <<(that: MuldivRsOutputBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
    that.ready := this.ready
  }
}

class MuldivRS(robConfig: RobConfig, rsConfig: RSConfig) extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val inputPorts = slave(MuldivRsInputBus(robConfig))
    val outputPorts = master(MuldivRsOutputBus(robConfig))
    val bypassPorts = Vec(slave(BypassInputBus(robConfig)), 9)
  }
  noIoPrefix()

  import io._

  val count = RegInit(U(0, log2Up(rsConfig.muldiv) bits))
  val rs = Vec(Reg(MuldivRsItem(robConfig)), rsConfig.muldiv)
  for (i <- 0 until rsConfig.muldiv) {
    rs(i).valid init False
  }
  inputPorts.ready(0) := count < U(rsConfig.muldiv - 1, log2Up(rsConfig.muldiv) bits)
  inputPorts.ready(1) := count < U(rsConfig.muldiv - 2, log2Up(rsConfig.muldiv) bits)

  val rsWithInput = Vec(MuldivRsItem(robConfig), rsConfig.muldiv)
  val countWithInput = UInt(log2Up(rsConfig.muldiv) bits)
  for (i <- 0 until rsConfig.muldiv) {
    rsWithInput(i) := rs(i)
  }
  countWithInput := count
  when(inputPorts.valid(0) && inputPorts.valid(1)) {
    rsWithInput(count) := inputPorts.payload(0)
    rsWithInput(count + 1) := inputPorts.payload(1)
    countWithInput := count + 2
  }.elsewhen(inputPorts.valid(0) && !inputPorts.valid(1)) {
    rsWithInput(count) := inputPorts.payload(0)
    countWithInput := count + 1
  }.elsewhen(!inputPorts.valid(0) && inputPorts.valid(1)) {
    rsWithInput(count) := inputPorts.payload(1)
    countWithInput := count + 1
  }

  val rsWithBypass = Vec(MuldivRsItem(robConfig), rsConfig.muldiv)
  val rsBypassSignal = Vec(Vec(Vec(Bool(), 9), 2), rsConfig.muldiv)
  val byPassData = Vec(UInt(32 bits), 9)
  for (i <- 0 until 9) byPassData(i) := bypassPorts(i).data
  for (i <- 0 until rsConfig.muldiv) {
    for (j <- 0 until 2) {
      for (k <- 0 until 9) {
        rsBypassSignal(i)(j)(k) := rsWithInput(i).valid && rsWithInput(i).src(j).valid && rsWithInput(i).src(j).addr === bypassPorts(k).addr && bypassPorts(k).valid
      }
    }
  }
  for (i <- 0 until rsConfig.muldiv) {
    rsWithBypass(i).valid := rsWithInput(i).valid
    rsWithBypass(i).pc := rsWithInput(i).pc
    rsWithBypass(i).dest := rsWithInput(i).dest
    rsWithBypass(i).ctrl := rsWithInput(i).ctrl
    for (j <- 0 until 2) {
      rsWithBypass(i).src(j).valid := rsWithInput(i).src(j).valid
      rsWithBypass(i).src(j).addr := rsWithInput(i).src(j).addr
      rsWithBypass(i).src(j).wake := rsBypassSignal(i)(j).orR | rsWithInput(i).src(j).wake
      rsWithBypass(i).src(j).available := rsBypassSignal(i)(j).orR | rsWithInput(i).src(j).available
      rsWithBypass(i).src(j).data := MuxOH(
        Vec(
          rsWithInput(i).src(j).wake,
          !rsWithInput(i).src(j).wake && rsBypassSignal(i)(j).orR,
          !rsWithInput(i).src(j).wake && !rsBypassSignal(i)(j).orR
        ),
        Vec(
          rsWithInput(i).src(j).data,
          MuxOH(rsBypassSignal(i)(j), byPassData),
          U(0, 32 bits)
        )
      )
    }
  }

  val issue = rs(0).valid && rs(0).src(0).wake && rs(0).src(1).wake
  val issueTarget = 0
  outputPorts.valid := issue
  outputPorts.payload := rsWithBypass(issueTarget)

  val pop = outputPorts.valid && outputPorts.ready
  when(flush) {
    count := 0
    for (i <- 0 until rsConfig.muldiv) {
      rs(i).valid.clear()
    }
  }.otherwise {
    when(pop) {
      for (i <- 0 until rsConfig.muldiv) {
        rs(i) := rsWithBypass(i)
        if (i > 0) {
          when(U(i, log2Up(robConfig.lines) bits) > issueTarget) {
            rs(i - 1) := rsWithBypass(i)
          }
        }
      }
      rs(U(rsConfig.muldiv - 1, log2Up(rsConfig.muldiv) bits)).valid.clear()
      count := countWithInput - 1
    }.otherwise {
      for (i <- 0 until rsConfig.muldiv) {
        rs(i) := rsWithBypass(i)
      }
      count := countWithInput
    }
  }
}

object MuldivRSVerilog extends App {
  Config.spinal.generateVerilog(new MuldivRS(Config.robConfig, Config.rsConfig))
}