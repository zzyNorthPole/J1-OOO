package j1ooo.cpu.execute.normal

import j1ooo.cpu.decode.{AluCtrlBus, JuCtrlBus, TuCtrlBus}
import j1ooo.cpu.signal.InstType
import j1ooo.gen.{Config, RSConfig, RobConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, MuxOH, PriorityMux, master, slave, traversableOnceBoolPimped}

case class NormalRsItem(robConfig: RobConfig) extends Bundle {
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
  val pc, offset, predictPc = UInt(32 bits)
  val ctrl = new Bundle {
    val alu = AluCtrlBus()
    val ju = JuCtrlBus()
    val tu = TuCtrlBus()
    val instType = InstType()
  }
}

case class NormalRsInputBus(robConfig: RobConfig, rsConfig: RSConfig) extends Bundle with IMasterSlave {
  val valid = Vec(Bool(), 2)
  val ready = Vec(Bool(), 2)
  val payload = Vec(NormalRsItem(robConfig), 2)
  val count = UInt(log2Up(rsConfig.normal) bits)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready, count)
  }

  def << (that: NormalRsInputBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
    that.ready := this.ready
    that.count := this.count
  }
}

case class NormalRsOutputBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val payload = NormalRsItem(robConfig)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready)
  }

  def << (that: NormalRsOutputBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
    that.ready := this.ready
  }
}

case class BypassInputBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(log2Up(robConfig.lines) bits)
  val data = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, addr, data)
  }

  def << (that: BypassInputBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
    this.data := that.data
  }
}

class NormalRS(robConfig: RobConfig, rsConfig: RSConfig) extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val inputPorts = slave(NormalRsInputBus(robConfig, rsConfig))
    val outputPorts = master(NormalRsOutputBus(robConfig))
    val bypassPorts = Vec(slave(BypassInputBus(robConfig)), 9)
  }
  noIoPrefix()

  import io._

  val count = RegInit(U(0, log2Up(rsConfig.normal) bits))
  val rs = Vec(Reg(NormalRsItem(robConfig)), rsConfig.normal)
  for (i <- 0 until rsConfig.normal) {
    rs(i).valid init False
    rs(i).src(0).wake init False
    rs(i).src(0).valid init False
    rs(i).src(1).wake init False
    rs(i).src(1).valid init False
  }
  inputPorts.count := count
  // ready0 for 1 push, ready1 for 2 push
  inputPorts.ready(0) := count < U(rsConfig.normal - 1, log2Up(rsConfig.normal + 1) bits)
  inputPorts.ready(1) := count < U(rsConfig.normal - 2, log2Up(rsConfig.normal + 1) bits)

  val rsWithInput = Vec(NormalRsItem(robConfig), rsConfig.normal)
  val countWithInput = UInt(log2Up(rsConfig.normal) bits)
  for (i <- 0 until rsConfig.normal) {
    rsWithInput(i) := rs(i)
  }
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
  }.otherwise {
    countWithInput := count
  }

  val rsWithBypass = Vec(NormalRsItem(robConfig), rsConfig.normal)
  val rsBypassSignal = Vec(Vec(Vec(Bool(), 9), 2), rsConfig.normal)
  val byPassData = Vec(UInt(32 bits), 9)
  for (i <- 0 until 9) byPassData(i) := bypassPorts(i).data
  for (i <- 0 until rsConfig.normal) {
    for (j <- 0 until 2) {
      for (k <- 0 until 9) {
        rsBypassSignal(i)(j)(k) := rsWithInput(i).valid && rsWithInput(i).src(j).valid && rsWithInput(i).src(j).addr === bypassPorts(k).addr && bypassPorts(k).valid
      }
    }
  }
  for (i <- 0 until rsConfig.normal) {
    rsWithBypass(i).valid := rsWithInput(i).valid
    rsWithBypass(i).pc := rsWithInput(i).pc
    rsWithBypass(i).dest := rsWithInput(i).dest
    rsWithBypass(i).offset := rsWithInput(i).offset
    rsWithBypass(i).predictPc := rsWithInput(i).predictPc
    rsWithBypass(i).ctrl := rsWithInput(i).ctrl
    for (j <- 0 until 2) {
      rsWithBypass(i).src(j).valid := rsWithInput(i).src(j).valid
      rsWithBypass(i).src(j).addr := rsWithInput(i).src(j).addr
      rsWithBypass(i).src(j).wake := rsBypassSignal(i)(j).orR | rsWithInput(i).src(j).wake
      rsWithBypass(i).src(j).available := rsBypassSignal(i)(j).orR | rsWithInput(i).src(j).available
      rsWithBypass(i).src(j).data := MuxOH(
        Vec(
          rsWithInput(i).src(j).wake,
          rsBypassSignal(i)(j).orR && !rsWithInput(i).src(j).wake,
          !rsBypassSignal(i)(j).orR && !rsWithInput(i).src(j).wake
        ),
        Vec(
          rsWithInput(i).src(j).data,
          MuxOH(rsBypassSignal(i)(j), byPassData),
          U(0, 32 bits)
        )
      )
    }
  }

  val issues = Vec(Bool(), rsConfig.normal)
  for (i <- 0 until rsConfig.normal) {
    issues(i) := rs(i).valid && rs(i).src(0).wake && rs(i).src(1).wake
  }
  val issue = issues.orR
  val issueTarget = PriorityMux(
    issues,
    for (i <- 0 until rsConfig.normal) yield U(i, log2Up(rsConfig.normal) bits)
  )
  outputPorts.valid := issue
  outputPorts.payload := rsWithBypass(issueTarget)

  val pop = outputPorts.valid && outputPorts.ready
  when(flush) {
    count := 0
    for (i <- 0 until rsConfig.normal) {
      rs(i).valid.clear()
    }
  }.otherwise {
    when(pop) {
      for (i <- 0 until rsConfig.normal) {
        rs(i) := rsWithBypass(i)
        if (i > 0) {
          when(U(i, log2Up(rsConfig.normal) bits) > issueTarget) {
            rs(i - 1) := rsWithBypass(i)
          }
        }
      }
      rs(U(rsConfig.normal - 1, log2Up(rsConfig.normal) bits)).valid.clear()
      count := countWithInput - 1
    }.otherwise {
      for (i <- 0 until rsConfig.normal) {
        rs(i) := rsWithBypass(i)
      }
      count := countWithInput
    }
  }
}

object NormalRSVerilog extends App {
  Config.spinal.generateVerilog(new NormalRS(Config.robConfig, Config.rsConfig))
}