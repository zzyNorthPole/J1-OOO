package j1ooo.cpu.commit

import j1ooo.cpu.execute.{ArfCommitBus, RatBus, RobCommitBus}
import j1ooo.cpu.execute.memory.MSHRToCommitBus
import j1ooo.cpu.execute.muldiv.HiLoWriteBus
import j1ooo.cpu.fetch.{FetchException, ICacheOpInputBus}
import j1ooo.cpu.fetch.bpu.{CommitToBpuBus, RasItem}
import j1ooo.cpu.mmu.{Cp0ToMmuBus, TLB, TLBQueryBus}
import j1ooo.cpu.signal.{Exception, InstType, TlbOp}
import j1ooo.gen.{BpuConfig, CacheConfig, Config, RobConfig, TlbConfig}
import spinal.core._
import spinal.lib.misc.pipeline.{CtrlLink, StageLink}
import spinal.lib.{IMasterSlave, master, slave}

case class DebugSignal() extends Bundle with IMasterSlave {
  val aclk = Bool()
  val wb = new Bundle {
    val pc = UInt(32 bits)
    val rf = new Bundle {
      val wen = Bits(4 bits)
      val wnum = UInt(5 bits)
      val wdata = UInt(32 bits)
    }
  }
  val cp0 = new Bundle {
    val count = UInt(32 bits)
    val random = UInt(32 bits)
    val cause = UInt(32 bits)
  }
  val int = Bool()
  val commit = Bool()

  override def asMaster(): Unit = {
    in(aclk)
    out(wb, cp0, int, commit)
  }

  def << (that: DebugSignal): Unit = {
    this.aclk := that.aclk
    that.wb := this.wb
    that.cp0 := this.cp0
    that.int := this.int
    that.commit := this.commit
  }
}

class CommitPipeline(useDebug: Boolean, tlbConfig: TlbConfig, iCacheConfig: CacheConfig, dCacheConfig: CacheConfig, robConfig: RobConfig, bpuConfig: BpuConfig, decodePorts: Int) extends Component {
  val io = new Bundle {
    val extInt = in Bits(5 bits)

    val flush = out Bool()
    val targetPc = out UInt(32 bits)
    val curRas = out(RasItem(bpuConfig.rasConfig))

    val commitPorts = slave(RobCommitBus(robConfig, bpuConfig, decodePorts))

    val mmu = master(Cp0ToMmuBus())

    val mfc0 = slave(PipeToCp0BusMfc0())

    val tlbQuery = Vec(slave(TLBQueryBus(tlbConfig)), 3)

    val mshr = master(MSHRToCommitBus(robConfig))
    val mshrToRob = slave(MSHRToCommitBus(robConfig))
    val cachedStore = new Bundle {
      val valid = out Vec(Bool(), 2)
      val ready = in Bool()
    }

    val arf = master(ArfCommitBus(decodePorts))

    val rat = Vec(master(RatBus(robConfig, "commit")), decodePorts)

    val hilo = Vec(master(HiLoWriteBus()), decodePorts)

    val bpu = master(CommitToBpuBus(bpuConfig))

    val icache = master(ICacheOpInputBus())

    val debug = (useDebug == true) generate master(DebugSignal())
  }
  noIoPrefix()
  import io._

  val cp0 = new Cp0(useDebug, tlbConfig, iCacheConfig, dCacheConfig)
  val tlb = new TLB(tlbConfig)

  flush.clear()
  targetPc.clearAll()
  cp0.io.pipe.eret.clear()
  cp0.io.pipe.tlbOp.clearAll()
  cp0.io.pipe.mtc0.clearAll()
  cp0.io.pipe.mov.clearAll()

  curRas := commitPorts.payload(1).predict.curRas

  mmu << cp0.io.mmu
  cp0.io.pipe.mfc0 << mfc0

  tlb.io.wPorts << cp0.io.tlb.wPorts
  tlb.io.rPorts << cp0.io.tlb.rPorts
  for (i <- 0 until 3) {
    tlb.io.queryPorts(i) << tlbQuery(i)
  }
  tlb.io.tlbpPorts << cp0.io.tlb.pPorts

  cp0.io.extInt := extInt
  val exDescription0 = FetchException()
  exDescription0 := commitPorts.payload(0).exDescription
  when(commitPorts.payload(0).instType =/= InstType.ERET && cp0.io.interrupt) {
    exDescription0.valid := True
    exDescription0.op := Exception.INT()
    exDescription0.isTlbHit := False
  }
  cp0.io.pipe.exception.clearAll()

  mshr.valid.clear()
  mshrToRob.ready := mshr.ready && mshr.valid
  mshrToRob.addr := mshr.addr
  mshrToRob.data := mshr.data
  cachedStore.valid(0).clear()
  cachedStore.valid(1).clear()

  for (i <- 0 until decodePorts) {
    arf.valid(i) := commitPorts.valid(i) && commitPorts.ready(i) && commitPorts.payload(i).archDest.valid && !(
      if (i == 0) exDescription0.valid else commitPorts.payload(i).exDescription.valid
    )
    arf.addr(i) := commitPorts.payload(i).archDest.addr
    arf.data(i) := commitPorts.payload(i).archDest.dout
    hilo(i).valid := commitPorts.valid(i) && commitPorts.ready(i) && commitPorts.payload(i).instType === InstType.MDU() && ! (
      if (i == 0) exDescription0.valid else commitPorts.payload(i).exDescription.valid
    )
    hilo(i).op := commitPorts.payload(i).mduOp
    hilo(i).din := commitPorts.payload(i).hiloDin
    rat(i).valid := commitPorts.valid(i) && commitPorts.ready(i) && commitPorts.payload(i).archDest.valid && !(
      if (i == 0) exDescription0.valid else commitPorts.payload(i).exDescription.valid
    )
    rat(i).ratAddr := commitPorts.payload(i).archDest.addr
    rat(i).ratItem.inRob := True
    rat(i).ratItem.robAddr := commitPorts.payload(i).phyDest
  }

  bpu.valid := commitPorts.valid(0) && commitPorts.ready(0) && commitPorts.payload(0).instType === InstType.JU() && !exDescription0.valid
  bpu.pc := commitPorts.payload(0).pc
  bpu.op := commitPorts.payload(0).predict.op
  bpu.rs := commitPorts.payload(0).predict.rs
  bpu.success := !commitPorts.payload(0).predictRes.fail
  bpu.target := commitPorts.payload(0).predict.targetPc
  bpu.history := commitPorts.payload(0).predict.history
  bpu.count := commitPorts.payload(0).predict.count

  icache.valid := (
    (commitPorts.valid(0) && commitPorts.ready(0) && commitPorts.payload(0).instType === InstType.CACHEI) ||
      (commitPorts.valid(1) && commitPorts.ready(1) && commitPorts.payload(1).instType === InstType.CACHEI)
    )
  icache.addr := (commitPorts.payload(0).instType === InstType.JU()) ? commitPorts.payload(1).addr | commitPorts.payload(0).addr

  // first: TU/MTC0/TLB/ERET/exception submit only
  // first: ALU/MDU/MFC0/LOAD/STORE/CACHED/CACHEI/OTHERS conditional submit both
  // first: JU must submit both
  commitPorts.ready(0).clear()
  commitPorts.ready(1).clear()

  when(commitPorts.valid(0) && commitPorts.payload(0).finish) {
    when(commitPorts.payload(0).instType === InstType.ERET) {
      commitPorts.ready(0).set()
      flush.set()
      targetPc := cp0.io.pcReg.pc
      cp0.io.pipe.eret.set()
    }.elsewhen(exDescription0.valid) {
      commitPorts.ready(0).set()
      flush.set()
      targetPc := cp0.io.pcReg.pc
      cp0.io.pipe.exception.valid := exDescription0.valid
      cp0.io.pipe.exception.op := exDescription0.op
      cp0.io.pipe.exception.isTlbHit := exDescription0.isTlbHit
      cp0.io.pipe.exception.bad.pc := commitPorts.payload(0).pc
      cp0.io.pipe.exception.bad.delaySlot.clear()
      cp0.io.pipe.exception.bad.addr := commitPorts.payload(0).exBadAddr
      cp0.io.pipe.exception.bad.vpn2 := commitPorts.payload(0).exBadAddr(31 downto 13)
    }.elsewhen(
      commitPorts.payload(0).instType === InstType.TLB() ||
        commitPorts.payload(0).instType === InstType.MTC0() ||
        (commitPorts.payload(0).instType === InstType.ALU() && commitPorts.payload(0).movFail)
    ) {
      commitPorts.ready(0).set()
      flush.set()
      targetPc := cp0.io.pcReg.pc
      cp0.io.pipe.tlbOp.valid := commitPorts.payload(0).instType === InstType.TLB()
      cp0.io.pipe.tlbOp.tlbp.valid := (commitPorts.payload(0).instType === InstType.TLB()) && (commitPorts.payload(0).tlbOp === TlbOp.TLBP())
      cp0.io.pipe.tlbOp.tlbr.valid := (commitPorts.payload(0).instType === InstType.TLB()) && (commitPorts.payload(0).tlbOp === TlbOp.TLBR())
      cp0.io.pipe.tlbOp.tlbw.valid := (commitPorts.payload(0).instType === InstType.TLB()) && (commitPorts.payload(0).tlbOp === TlbOp.TLBWI() || commitPorts.payload(0).tlbOp === TlbOp.TLBWR())
      cp0.io.pipe.tlbOp.tlbw.isWr := commitPorts.payload(0).tlbOp === TlbOp.TLBWR()
      cp0.io.pipe.tlbOp.pc := commitPorts.payload(0).pc
      cp0.io.pipe.mtc0.valid := commitPorts.payload(0).instType === InstType.MTC0()
      cp0.io.pipe.mtc0.addr := commitPorts.payload(0).mtc0.addr
      cp0.io.pipe.mtc0.select := commitPorts.payload(0).mtc0.select
      cp0.io.pipe.mtc0.din := commitPorts.payload(0).mtc0.din1
      cp0.io.pipe.mtc0.pc := commitPorts.payload(0).pc
      cp0.io.pipe.mov.valid := (commitPorts.payload(0).instType === InstType.ALU() && commitPorts.payload(0).movFail)
      cp0.io.pipe.mov.pc := commitPorts.payload(0).pc
    }.elsewhen(
      (commitPorts.payload(0).instType === InstType.LOAD() ||
        commitPorts.payload(0).instType === InstType.STORE() ||
        commitPorts.payload(0).instType === InstType.CACHED()) &&
        !commitPorts.payload(0).complete
    ) {
      commitPorts.ready(0).clear()
      mshr.valid.set()
    }.elsewhen(commitPorts.payload(0).instType === InstType.JU()) {
      commitPorts.ready(0).clear()
      when(commitPorts.valid(1) && commitPorts.payload(1).finish) {
        when(commitPorts.payload(1).exDescription.valid) {
          commitPorts.ready(0).set()
          commitPorts.ready(1).set()
          flush.set()
          targetPc := cp0.io.pcReg.pc
          cp0.io.pipe.exception.valid := commitPorts.payload(1).exDescription.valid
          cp0.io.pipe.exception.op := commitPorts.payload(1).exDescription.op
          cp0.io.pipe.exception.isTlbHit := commitPorts.payload(1).exDescription.isTlbHit
          cp0.io.pipe.exception.bad.pc := commitPorts.payload(1).pc
          cp0.io.pipe.exception.bad.delaySlot.set()
          cp0.io.pipe.exception.bad.addr := commitPorts.payload(1).exBadAddr
          cp0.io.pipe.exception.bad.vpn2 := commitPorts.payload(1).exBadAddr(31 downto 13)
        }.elsewhen(
          commitPorts.payload(1).instType === InstType.TLB() ||
            commitPorts.payload(1).instType === InstType.MTC0() ||
            (commitPorts.payload(1).instType === InstType.ALU() && commitPorts.payload(1).movFail)
        ) {
          commitPorts.ready(0).set()
          commitPorts.ready(1).set()
          flush.set()
          targetPc := commitPorts.payload(0).predictRes.nextPc
          cp0.io.pipe.tlbOp.valid := commitPorts.payload(1).instType === InstType.TLB()
          cp0.io.pipe.tlbOp.tlbp.valid := (commitPorts.payload(1).instType === InstType.TLB()) && (commitPorts.payload(1).tlbOp === TlbOp.TLBP())
          cp0.io.pipe.tlbOp.tlbr.valid := (commitPorts.payload(1).instType === InstType.TLB()) && (commitPorts.payload(1).tlbOp === TlbOp.TLBR())
          cp0.io.pipe.tlbOp.tlbw.valid := (commitPorts.payload(1).instType === InstType.TLB()) && (commitPorts.payload(1).tlbOp === TlbOp.TLBWI() || commitPorts.payload(1).tlbOp === TlbOp.TLBWR())
          cp0.io.pipe.tlbOp.tlbw.isWr := commitPorts.payload(1).tlbOp === TlbOp.TLBWR()
          cp0.io.pipe.tlbOp.pc := commitPorts.payload(1).pc
          cp0.io.pipe.mtc0.valid := commitPorts.payload(1).instType === InstType.MTC0()
          cp0.io.pipe.mtc0.addr := commitPorts.payload(1).mtc0.addr
          cp0.io.pipe.mtc0.select := commitPorts.payload(1).mtc0.select
          cp0.io.pipe.mtc0.din := commitPorts.payload(1).mtc0.din1
          cp0.io.pipe.mtc0.pc := commitPorts.payload(1).pc
          cp0.io.pipe.mov.valid := (commitPorts.payload(1).instType === InstType.ALU() && commitPorts.payload(1).movFail)
          cp0.io.pipe.mov.pc := commitPorts.payload(1).pc
        }.elsewhen(
          (commitPorts.payload(1).instType === InstType.LOAD() ||
            commitPorts.payload(1).instType === InstType.STORE() ||
            commitPorts.payload(1).instType === InstType.CACHED()) &&
            !commitPorts.payload(1).complete
        ) {
          commitPorts.ready(0).clear()
          commitPorts.ready(1).clear()
          mshr.valid.set()
        }.elsewhen(
          (commitPorts.payload(1).instType === InstType.STORE() &&
            commitPorts.payload(1).isCached && commitPorts.payload(1).complete)
        ) {
          cachedStore.valid(0).set()
          when(cachedStore.ready) {
            commitPorts.ready(0).set()
            commitPorts.ready(1).set()
            when(commitPorts.payload(0).predictRes.fail) {
              flush.set()
              targetPc := commitPorts.payload(0).predictRes.nextPc
              // TODO
            }
          }
        }.otherwise {
          commitPorts.ready(0).set()
          commitPorts.ready(1).set()
          when(commitPorts.payload(0).predictRes.fail) {
            flush.set()
            targetPc := commitPorts.payload(0).predictRes.nextPc
            // TODO
          }
        }
      }
    }.elsewhen(commitPorts.payload(0).instType === InstType.STORE() && commitPorts.payload(0).isCached && commitPorts.payload(0).complete) {
      cachedStore.valid(0).set()
      when(cachedStore.ready) {
        commitPorts.ready(0).set()
      }
    }.otherwise {
      commitPorts.ready(0).set()
      when(commitPorts.valid(1) && commitPorts.payload(1).finish) {
        when(commitPorts.payload(1).instType === InstType.ERET) {
          commitPorts.ready(1).set()
          flush.set()
          targetPc := cp0.io.pcReg.pc
          cp0.io.pipe.eret.set()
        }.elsewhen(commitPorts.payload(1).exDescription.valid) {
          commitPorts.ready(1).set()
          flush.set()
          targetPc := cp0.io.pcReg.pc
          cp0.io.pipe.exception.valid := commitPorts.payload(1).exDescription.valid
          cp0.io.pipe.exception.op := commitPorts.payload(1).exDescription.op
          cp0.io.pipe.exception.isTlbHit := commitPorts.payload(1).exDescription.isTlbHit
          cp0.io.pipe.exception.bad.pc := commitPorts.payload(1).pc
          cp0.io.pipe.exception.bad.delaySlot.set()
          cp0.io.pipe.exception.bad.addr := commitPorts.payload(1).exBadAddr
          cp0.io.pipe.exception.bad.vpn2 := commitPorts.payload(1).exBadAddr(31 downto 13)
        }.elsewhen(
          commitPorts.payload(1).instType === InstType.TLB() ||
            commitPorts.payload(1).instType === InstType.MTC0() ||
            (commitPorts.payload(1).instType === InstType.ALU() && commitPorts.payload(1).movFail)
        ) {
          commitPorts.ready(1).set()
          flush.set()
          targetPc := cp0.io.pcReg.pc
          cp0.io.pipe.tlbOp.valid := commitPorts.payload(1).instType === InstType.TLB()
          cp0.io.pipe.tlbOp.tlbp.valid := (commitPorts.payload(1).instType === InstType.TLB()) && (commitPorts.payload(1).tlbOp === TlbOp.TLBP())
          cp0.io.pipe.tlbOp.tlbr.valid := (commitPorts.payload(1).instType === InstType.TLB()) && (commitPorts.payload(1).tlbOp === TlbOp.TLBR())
          cp0.io.pipe.tlbOp.tlbw.valid := (commitPorts.payload(1).instType === InstType.TLB()) && (commitPorts.payload(1).tlbOp === TlbOp.TLBWI() || commitPorts.payload(1).tlbOp === TlbOp.TLBWR())
          cp0.io.pipe.tlbOp.tlbw.isWr := commitPorts.payload(1).tlbOp === TlbOp.TLBWR()
          cp0.io.pipe.tlbOp.pc := commitPorts.payload(1).pc
          cp0.io.pipe.mtc0.valid := commitPorts.payload(1).instType === InstType.MTC0()
          cp0.io.pipe.mtc0.addr := commitPorts.payload(1).mtc0.addr
          cp0.io.pipe.mtc0.select := commitPorts.payload(1).mtc0.select
          cp0.io.pipe.mtc0.din := commitPorts.payload(1).mtc0.din1
          cp0.io.pipe.mtc0.pc := commitPorts.payload(1).pc
          cp0.io.pipe.mov.valid := (commitPorts.payload(1).instType === InstType.ALU() && commitPorts.payload(1).movFail)
          cp0.io.pipe.mov.pc := commitPorts.payload(1).pc
        }.elsewhen(
          (commitPorts.payload(1).instType === InstType.LOAD() ||
            commitPorts.payload(1).instType === InstType.STORE() ||
            commitPorts.payload(1).instType === InstType.CACHED()) &&
            !commitPorts.payload(1).complete
        ) {
          commitPorts.ready(1).clear()
        }.elsewhen(commitPorts.payload(1).instType === InstType.JU()) {
          commitPorts.ready(1).clear()
        }.elsewhen(commitPorts.payload(1).instType === InstType.CACHEI()) {
          commitPorts.ready(1).clear()
        }.elsewhen(
          (commitPorts.payload(1).instType === InstType.STORE() &&
            commitPorts.payload(1).isCached && commitPorts.payload(1).complete)
        ) {
          cachedStore.valid(0).set()
          when(cachedStore.ready) {
            commitPorts.ready(1).set()
          }
        }.otherwise {
          // TODO
          commitPorts.ready(1).set()
        }
      }
    }
  }

  (useDebug == true) generate {
    debug.wb.pc := debug.aclk ? commitPorts.payload(0).pc | (!exDescription0.valid ? commitPorts.payload(1).pc | U(0, 32 bits))
    debug.wb.rf.wen := debug.aclk ? (
      B(4 bits, default -> (
        commitPorts.valid(0) && commitPorts.ready(0) && commitPorts.payload(0).archDest.valid && !exDescription0.valid
        ))
      ) | (
      B(4 bits, default -> (
        commitPorts.valid(1) && commitPorts.ready(1) && commitPorts.payload(1).archDest.valid && !exDescription0.valid && !commitPorts.payload(1).exDescription.valid
        ))
      )
    debug.wb.rf.wnum := debug.aclk ? commitPorts.payload(0).archDest.addr | commitPorts.payload(1).archDest.addr
    debug.wb.rf.wdata := debug.aclk ? commitPorts.payload(0).archDest.dout | commitPorts.payload(1).archDest.dout
    debug.cp0.count := cp0.io.debug.count
    debug.cp0.random := cp0.io.debug.random
    debug.cp0.cause := cp0.io.debug.cause
    debug.int := commitPorts.valid(0) && commitPorts.ready(0) && exDescription0.valid && (exDescription0.op === Exception.INT())
    debug.commit := commitPorts.valid(0) && commitPorts.ready(0)
  }
}

object CommitPipelineVerilog extends App {
  Config.spinal.generateVerilog(new CommitPipeline(
    useDebug = true,
    tlbConfig = Config.tlbConfig,
    iCacheConfig = Config.iCacheConfig,
    dCacheConfig = Config.dCacheConfig,
    robConfig = Config.robConfig,
    bpuConfig = Config.bpuConfig,
    decodePorts = 2
  ))
}