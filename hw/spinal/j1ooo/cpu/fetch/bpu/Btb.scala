package j1ooo.cpu.fetch.bpu

import j1ooo.cpu.blackbox.RamParameter
import j1ooo.cpu.util.Ram
import j1ooo.gen.BtbConfig
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class BtbRes() extends Bundle with IMasterSlave {
  // valid used as validate signal in commit stage
  // valid used as mark of tag matching
  val valid = Bool()
  val target = UInt(32 bits)
  val isBranch = Bool()
  val isJump = Bool()
  val isCall = Bool()
  val isRet = Bool()

  override def asMaster(): Unit = {
    out(valid)
    out(target)
    out(isBranch, isJump, isCall, isRet)
  }
}

case class BtbBus(_type: String) extends Bundle with IMasterSlave {
  val pc = UInt(32 bits)
  val res = BtbRes()

  override def asMaster(): Unit = {
    out(pc)
    (_type == "read") generate in(res)
    (_type == "write") generate out(res)
  }
}

class Btb(btbConfig: BtbConfig, _type: String) extends Component {
  val io = new Bundle {
    val rPorts = Vec(slave(BtbBus("read")), btbConfig.ways)
    val wPort = slave(BtbBus("write"))
  }
  noIoPrefix()

  import io._

  val tagRams = Seq.fill(btbConfig.ways) {
    Ram(_type, RamParameter(
      btbConfig.lines, btbConfig.tagWidth, false, "dpdistram"
    ))
  }

  val validRams = Seq.fill(btbConfig.ways) {
    Ram("vec", RamParameter(
      btbConfig.lines, 1, false, "dpdistram"
    ))
  }

  // 00 B
  // 01 J
  // 10 JAL / JALR
  // 11 JR
  val opRams = Seq.fill(btbConfig.ways) {
    Ram("vec", RamParameter(
      btbConfig.lines, 2, false, "dpdistram"
    ))
  }

  val targetRams = Seq.fill(btbConfig.ways) {
    Ram(_type, RamParameter(
      btbConfig.lines, 32, false, "dpdistram"
    ))
  }

  // predict target:
  // 1.no branch + no branch: pc and (pc + 4)
  // 2.branch + delay slot: pc and (pc + 4)
  // 3.no branch + branch: pc and (pc + 4)
  // 4.delay slot + jump target: pc and ?
  // 1.2.3. in different part
  // 4. maybe in same part, but ignore delay slot as it is impossible to be branch inst
  val readPortsInit = new Area {
    val pcUpd = Vec(UInt(32 bits), btbConfig.ways)
    pcUpd(1) := rPorts(1).pc(2) ? rPorts(1).pc | rPorts(0).pc
    pcUpd(0) := rPorts(1).pc(2) ? rPorts(0).pc | rPorts(1).pc

    val tags = Vec(UInt(btbConfig.tagWidth bits), btbConfig.ways)
    val indexes = Vec(UInt(btbConfig.indexWidth bits), btbConfig.ways)

    for (i <- 0 until 2) {
      tags(i) := pcUpd(i)(32 - btbConfig.tagWidth, btbConfig.tagWidth bits)
      indexes(i) := pcUpd(i)(3, btbConfig.indexWidth bits)
    }
    assert(32 - btbConfig.tagWidth == 3 + btbConfig.indexWidth)

    val curTags = Vec(UInt(btbConfig.tagWidth bits), btbConfig.ways)
    for (i <- 0 until btbConfig.ways) {
      tagRams(i).io.enb := True
      tagRams(i).io.addrb := indexes(i)
      curTags(i) := tagRams(i).io.doutb
    }

    val valids = Vec(Bool(), btbConfig.ways)
    for (i <- 0 until btbConfig.ways) {
      validRams(i).io.enb := True
      validRams(i).io.addrb := indexes(i)
      valids(i) := validRams(i).io.doutb.asBool
    }

    val hits = Vec(Bool(), btbConfig.ways)
    for (i <- 0 until btbConfig.ways) {
      hits(i) := (curTags(i) === tags(i)) && valids(i)
    }

    val ops = Vec(UInt(2 bits), btbConfig.ways)
    for (i <- 0 until btbConfig.ways) {
      opRams(i).io.enb := True
      opRams(i).io.addrb := indexes(i)
      ops(i) := opRams(i).io.doutb
    }

    val targets = Vec(UInt(32 bits), btbConfig.ways)
    for (i <- 0 until btbConfig.ways) {
      targetRams(i).io.enb := True
      targetRams(i).io.addrb := indexes(i)
      targets(i) := targetRams(i).io.doutb
    }

    rPorts(1).res.valid := rPorts(1).pc(2) ? hits(1) | hits(0)
    rPorts(0).res.valid := rPorts(1).pc(2) ? hits(0) | hits(1)
    rPorts(1).res.target := rPorts(1).pc(2) ? targets(1) | targets(0)
    rPorts(0).res.target := rPorts(1).pc(2) ? targets(0) | targets(1)

    val opUpd = Vec(UInt(2 bits), btbConfig.ways)
    opUpd(1) := rPorts(1).pc(2) ? ops(1) | ops(0)
    opUpd(0) := rPorts(1).pc(2) ? ops(0) | ops(1)
    val isBranch = Vec(Bool(), btbConfig.ways)
    val isJump = Vec(Bool(), btbConfig.ways)
    val isCall = Vec(Bool(), btbConfig.ways)
    val isRet = Vec(Bool(), btbConfig.ways)
    for (i <- 0 until btbConfig.ways) {
      isBranch(i) := opUpd(i) === U(0, 2 bits)
      isJump(i) := opUpd(i) === U(1, 2 bits)
      isCall(i) := opUpd(i) === U(2, 2 bits)
      isRet(i) := opUpd(i) === U(3, 2 bits)
    }
    rPorts(1).res.isBranch := rPorts(1).pc(2) ? isBranch(1) | isBranch(0)
    rPorts(0).res.isBranch := rPorts(1).pc(2) ? isBranch(0) | isBranch(1)
    rPorts(1).res.isJump := rPorts(1).pc(2) ? isJump(1) | isJump(0)
    rPorts(0).res.isJump := rPorts(1).pc(2) ? isJump(0) | isJump(1)
    rPorts(1).res.isCall := rPorts(1).pc(2) ? isCall(1) | isCall(0)
    rPorts(0).res.isCall := rPorts(1).pc(2) ? isCall(0) | isCall(1)
    rPorts(1).res.isRet := rPorts(1).pc(2) ? isRet(1) | isRet(0)
    rPorts(0).res.isRet := rPorts(1).pc(2) ? isRet(0) | isRet(1)
  }

  val writePortsInit = new Area {
    val tag = UInt(btbConfig.tagWidth bits)
    val index = UInt(btbConfig.indexWidth bits)
    tag := wPort.pc(32 - btbConfig.tagWidth, btbConfig.tagWidth bits)
    index := wPort.pc(3, btbConfig.indexWidth bits)
    for (i <- 0 until btbConfig.ways) {
      tagRams(i).io.ena := wPort.res.valid && (U(i, 1 bits) === wPort.pc(2, 1 bits))
      tagRams(i).io.wea := B(1, 1 bits)
      tagRams(i).io.addra := index
      tagRams(i).io.dina := tag
    }

    for (i <- 0 until btbConfig.ways) {
      validRams(i).io.ena := wPort.res.valid && (U(i, 1 bits) === wPort.pc(2, 1 bits))
      validRams(i).io.wea := B(1, 1 bits)
      validRams(i).io.addra := index
      validRams(i).io.dina := U(1, 1 bits)
    }

    for (i <- 0 until btbConfig.ways) {
      targetRams(i).io.ena := wPort.res.valid && (U(i, 1 bits) === wPort.pc(2, 1 bits))
      targetRams(i).io.wea := B(1, 1 bits)
      targetRams(i).io.addra := index
      targetRams(i).io.dina := wPort.res.target
    }

    val op = UInt(2 bits)
    op(1) := wPort.res.isCall | wPort.res.isRet
    op(0) := wPort.res.isJump | wPort.res.isRet
    for (i <- 0 until btbConfig.ways) {
      opRams(i).io.ena := wPort.res.valid && (U(i, 1 bits) === wPort.pc(2, 1 bits))
      opRams(i).io.wea := B(1, 1 bits)
      opRams(i).io.addra := index
      opRams(i).io.dina := op
    }
  }
}
