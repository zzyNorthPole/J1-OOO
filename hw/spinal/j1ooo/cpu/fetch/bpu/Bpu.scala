package j1ooo.cpu.fetch.bpu

import j1ooo.cpu.signal.JuOp
import j1ooo.gen.{BpuConfig, Config}
import spinal.core._
import spinal.lib.{IMasterSlave, MuxOH, slave}

case class PredictRes(bpuConfig: BpuConfig, fetchPorts: Int) extends Bundle with IMasterSlave {
  val predictPc = Vec(UInt(32 bits), fetchPorts)
  val history = Vec(UInt(bpuConfig.bhtConfig.histories bits), fetchPorts)
  val count = Vec(UInt(2 bits), fetchPorts)
  val curRas = RasItem(bpuConfig.rasConfig)

  override def asMaster(): Unit = {
    out(predictPc, history, count, curRas)
  }
}

case class PipeToBpuBus(bpuConfig: BpuConfig, fetchPorts: Int) extends Bundle with IMasterSlave {
  val ready = Bool()
  val pc = Vec(UInt(32 bits), fetchPorts)
  val res = PredictRes(bpuConfig, fetchPorts)

  override def asMaster(): Unit = {
    out(ready)
    out(pc)
    in(res)
  }
}

case class CommitToBpuBus(bpuConfig: BpuConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val pc = UInt(32 bits)
  // check operator
  val op = JuOp()
  // check is ret or not
  val rs = UInt(log2Up(32) bits)
  // branch predict fail or not
  val success = Bool()
  val target = UInt(32 bits)
  val history = UInt(bpuConfig.bhtConfig.histories bits)
  val count = UInt(2 bits)

  override def asMaster(): Unit = {
    out(valid, pc, op, rs, success, target, history, count)
  }

  def << (that: CommitToBpuBus): Unit = {
    this.valid := that.valid
    this.pc := that.pc
    this.op := that.op
    this.rs := that.rs
    this.success := that.success
    this.target := that.target
    this.history := that.history
    this.count := that.count
  }
}

class Bpu(bpuConfig: BpuConfig, fetchPorts: Int, _type: String) extends Component {
  import bpuConfig._
  val io = new Bundle {
    val rPorts = slave(PipeToBpuBus(bpuConfig, fetchPorts))
    val wPorts = slave(CommitToBpuBus(bpuConfig))
    val flushPorts = slave(RasRefreshBus(rasConfig))
  }
  noIoPrefix()

  import io._

  assert(fetchPorts == btbConfig.ways)
  assert(fetchPorts == bhtConfig.ways)
  assert(fetchPorts == phtConfig.fetchPorts)
  val btb = new Btb(btbConfig, _type)
  val bht = new Bht(bhtConfig, _type)
  val pht = new Pht(phtConfig)
  val ras = new Ras(rasConfig)

  val readPortsInit = new Area {
    // btb
    val btbRes = Vec(BtbRes(), fetchPorts)
    for (i <- 0 until fetchPorts) {
      btb.io.rPorts(i).pc := rPorts.pc(i)
      btbRes(i) := btb.io.rPorts(i).res
    }

    // bht
    val histories = Vec(UInt(bhtConfig.histories bits), fetchPorts)
    for (i <- 0 until fetchPorts) {
      bht.io.rPorts(i).pc := rPorts.pc(i)
      histories(i) := bht.io.rPorts(i).history
    }

    // pht
    val count = Vec(UInt(2 bits), fetchPorts)
    for (i <- 0 until fetchPorts) {
      pht.io.rPorts(i).history := rPorts.pc(i)(2, (phtConfig.indexWidth - bhtConfig.histories) bits) @@ histories(i)
      count(i) := pht.io.rPorts(i).count
    }

    // ras
    val rasTarget = UInt(32 bits)
    val curRas = RasItem(rasConfig)
    ras.io.queryPort.pc := btbRes(0).isRet ? rPorts.pc(0) | rPorts.pc(1)
    rasTarget := ras.io.queryPort.target
    curRas := ras.io.queryPort.curRas

    val pc0IsDelaySlot = !(rPorts.pc(0)(2) ^ rPorts.pc(1)(2))
    val btbRes0Valid = btbRes(0).valid && !pc0IsDelaySlot
    val nextPc = Vec(UInt(32 bits), fetchPorts)
    when(btbRes0Valid && !btbRes(1).valid) {
      nextPc(0) := MuxOH(
        Vec(
          btbRes(0).isBranch,
          btbRes(0).isJump | btbRes(0).isCall,
          btbRes(0).isRet
        ),
        Vec(
          count(0)(1) ? btbRes(0).target | (rPorts.pc(0) + 8),
          btbRes(0).target,
          rasTarget
        )
      )
      nextPc(1) := nextPc(0) + 4
    }.elsewhen(!btbRes0Valid && btbRes(1).valid) {
      nextPc(0) := rPorts.pc(1) + 4
      nextPc(1) := MuxOH(
        Vec(
          btbRes(1).isBranch,
          btbRes(1).isJump | btbRes(1).isCall,
          btbRes(1).isRet
        ),
        Vec(
          count(1)(1) ? btbRes(1).target | (rPorts.pc(1) + 8),
          btbRes(1).target,
          rasTarget
        )
      )
    }.otherwise {
      nextPc(0) := rPorts.pc(1) + 4
      nextPc(1) := rPorts.pc(1) + 8
    }
    rPorts.res.predictPc(0) := nextPc(0)
    rPorts.res.predictPc(1) := nextPc(1)

    // extra info
    rPorts.res.history := histories
    rPorts.res.count := count
    rPorts.res.curRas := curRas

    // ras upd when call/ret
    ras.io.pushPort.valid.clear()
    ras.io.popPort.valid.clear()
    when(rPorts.ready) {
      when(btbRes(0).isCall | btbRes(1).isCall) {
        ras.io.pushPort.valid.set()
      }
      when(btbRes(0).isRet | btbRes(1).isRet) {
        ras.io.popPort.valid.set()
      }
    }
    ras.io.pushPort.target := btbRes(0).isCall ? (rPorts.pc(0) + 8) | (rPorts.pc(1) + 8)
  }

  val writePortsInit = new Area {
    val isBranch = (wPorts.op =/= JuOp.J) || (wPorts.op =/= JuOp.JAL) || (wPorts.op =/= JuOp.JALR) || (wPorts.op =/= JuOp.JR)
    val isJump = (wPorts.op === JuOp.J) || (wPorts.op === JuOp.JR && wPorts.rs =/= U(31, 5 bits))
    val isCall = (wPorts.op === JuOp.JAL) || (wPorts.op === JuOp.JALR)
    val isRet = wPorts.op === JuOp.JR && wPorts.rs === U(31, 5 bits)
    btb.io.wPort.pc := wPorts.pc
    btb.io.wPort.res.valid := wPorts.valid
    btb.io.wPort.res.target := wPorts.target
    btb.io.wPort.res.isBranch := isBranch
    btb.io.wPort.res.isJump := isJump
    btb.io.wPort.res.isCall := isCall
    btb.io.wPort.res.isRet := isRet

    bht.io.wPort.pc := wPorts.pc
    bht.io.wPort.valid := wPorts.valid && isBranch
    val historyUpd = UInt(bhtConfig.histories bits)
    historyUpd := wPorts.history(1, (bhtConfig.histories - 1) bits) @@ wPorts.success.asUInt
    bht.io.wPort.history := historyUpd

    pht.io.wPort.history := wPorts.pc(2, (phtConfig.indexWidth - bhtConfig.histories) bits) @@ wPorts.history
    pht.io.wPort.valid := wPorts.valid && isBranch
    val countUpd = UInt(2 bits)
    when(wPorts.success) {
      when(wPorts.count === U(3, 2 bits)) {
        countUpd := wPorts.count
      }.otherwise {
        countUpd := wPorts.count + 1
      }
    }.otherwise {
      when(wPorts.count === U(0, 2 bits)) {
        countUpd := wPorts.count
      }.otherwise {
        countUpd := wPorts.count - 1
      }
    }
    pht.io.wPort.count := countUpd
  }

  val flushPortsInit = new Bundle {
    ras.io.flushPort.valid := flushPorts.valid
    ras.io.flushPort.item := flushPorts.item
  }
}

object BpuVerilog extends App {
  Config.spinal.generateVerilog(new Bpu(Config.bpuConfig, 2, "sim"))
}