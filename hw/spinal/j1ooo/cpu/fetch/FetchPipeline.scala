package j1ooo.cpu.fetch

import j1ooo.cpu.commit.PipeToCp0BusException
import j1ooo.cpu.fetch.bpu.{Bpu, CommitToBpuBus, PredictRes, RasItem}
import j1ooo.cpu.mmu.{Cp0ToMmuBus, Mmu, TLBQueryBus}
import j1ooo.cpu.signal.Exception
import j1ooo.gen.Config.{axi4Config, tlbConfig}
import j1ooo.gen.{BpuConfig, CacheConfig, Config, TlbConfig}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly}
import spinal.lib.{IMasterSlave, PriorityMux, master, slave}
import spinal.lib.misc.pipeline.{Builder, CtrlLink, StageLink}

case class FetchException() extends Bundle {
  val valid = Bool()
  val op = Exception()
  val isTlbHit = Bool()
}

case class FetchRes(bpuConfig: BpuConfig, fetchPorts: Int) extends Bundle {
  val pc = Vec(UInt(32 bits), fetchPorts)
  val inst = Vec(UInt(32 bits), fetchPorts)
  val predict = PredictRes(bpuConfig, fetchPorts)
  val exDescription = Vec(FetchException(), fetchPorts)
}

case class FetchOutputBus(bpuConfig: BpuConfig, fetchPorts: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val dout = FetchRes(bpuConfig, fetchPorts)

  override def asMaster(): Unit = {
    out(valid, dout)
    in(ready)
  }

  def << (that: FetchOutputBus): Unit = {
    this.valid := that.valid
    this.dout := that.dout
    that.ready := this.ready
  }
}


class FetchPipeline(
                   cacheConfig: CacheConfig,
                   bpuConfig: BpuConfig,
                   tlbConfig: TlbConfig,
                   fetchPortsNum: Int,
                   _type: String
                   ) extends Component {
  val io = new Bundle {
    // commit stage
    // flush: pcManage bpu iCache
    // write bpu
    // cache op clear iCache's cache line
    val flush = in Bool()
    val flushPc = in UInt(32 bits)
    val flushRas = in(RasItem(bpuConfig.rasConfig))
    // TODO
    val commitToBpuBus = slave(CommitToBpuBus(bpuConfig))
    val commitToICacheOpBus = slave(ICacheOpInputBus())

    // fetch1
    // mmu
    val cp0ToMmuBus = slave(Cp0ToMmuBus())
    val mmuToTlbBus = Vec(master(TLBQueryBus(tlbConfig)), fetchPortsNum)

    // fetch2
    // icache
    val iBus = master(Axi4ReadOnly(axi4Config))

    // fetch3
    val output = master(FetchOutputBus(bpuConfig, fetchPortsNum))
  }
  noIoPrefix()

  import io._

  val pcManager = new PcManager(fetchPortsNum)
  val mmu = new Mmu("I", tlbConfig, fetchPortsNum)
  val iCache = new ICache(cacheConfig, fetchPortsNum, _type)
  val bpu = new Bpu(bpuConfig, fetchPortsNum, _type)

  val commitAction = new Area {
    bpu.io.wPorts << commitToBpuBus
    iCache.io.cacheOpPort << commitToICacheOpBus
    bpu.io.flushPorts.valid := flush
    bpu.io.flushPorts.item := flushRas
  }
  // pc
  pcManager.io.flush := flush
  pcManager.io.flushPc := flushPc

  iCache.io.flush := flush

  val fetch1, fetch2, fetch3 = CtrlLink()
  val fetch1To2 = StageLink(fetch1.down, fetch2.up).withoutCollapse()
  val fetch2To3 = StageLink(fetch2.down, fetch3.up).withoutCollapse()

  val fetch1Action = new fetch1.Area {
    up.valid := True

    fetch1.throwWhen(flush, usingReady = true)

    // pc manager
    pcManager.io.ready := up.isReady
    val pc = Vec(UInt(32 bits), fetchPortsNum)
    pc := pcManager.io.pc
    val iAdEL = pcManager.io.addrErrorLoad

    // bpu
    bpu.io.rPorts.ready := up.isReady
    bpu.io.rPorts.pc := pc
    pcManager.io.nextPc := bpu.io.rPorts.res.predictPc
    val PC = insert(pc)
    val PREDICT = insert(bpu.io.rPorts.res)

    // mmu
    mmu.io.pipeToMmu.valid := up.isValid
    for (i <- 0 until fetchPortsNum) {
      mmu.io.pipeToMmu.isWrite(i) := False
      mmu.io.pipeToMmu.virtAddr(i) := pc(i)
    }
    mmu.io.cp0ToMmuBus << cp0ToMmuBus
    for (i <- 0 until fetchPortsNum) {
      mmuToTlbBus(i) << mmu.io.mmuToTlbBus(i)
    }
    val iTlbException = mmu.io.tlbException
    val exDescription = Vec(FetchException(), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      exDescription(i).valid := iAdEL(i) || iTlbException(i).refill || iTlbException(i).invalid
      exDescription(i).op := PriorityMux(
        Vec(
          iAdEL(i),
          iTlbException(i).refill || iTlbException(i).invalid
        ),
        Vec(
          Exception.ADEL(),
          Exception.TLBL()
        )
      )
      exDescription(i).isTlbHit := exDescription(i).valid && iTlbException(i).invalid
      // exDescription(i).bad.pc := pc(i)
      // exDescription(i).bad.delaySlot.clear()
      // exDescription(i).bad.addr := pc(i)
      // exDescription(i).bad.vpn2 := mmu.io.mmuToTlbBus(i).din.base.VPN2
    }
    val EX_DESCRIPTION = insert(exDescription)

    // icache
    iCache.io.fetchPorts.halt := !up.isReady
    iCache.io.fetchPorts.input.valid := up.isValid
    iCache.io.fetchPorts.input.cached := mmu.io.mmuToCacheBus.cached
    iCache.io.fetchPorts.input.addr := mmu.io.mmuToCacheBus.phyAddr
    val iCacheInputException = Vec(Bool(), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      iCacheInputException(i) := exDescription(i).valid
    }
    iCache.io.fetchPorts.input.exception := iCacheInputException
  }

  val fetch2Action = new fetch2.Area {
    fetch2.haltWhen(!iCache.io.fetchPorts.output.ready)
    fetch2.throwWhen(flush, usingReady = true)

    val instRegValid = RegInit(False)
    val instReg = Vec(RegInit(U(0, 32 bits)), fetchPortsNum)
    when(iCache.io.fetchPorts.output.valid && up.isValid) {
      instRegValid.set()
      instReg := iCache.io.fetchPorts.output.dout
    }
    when(flush || up.isReady) {
      instRegValid.clear()
    }
    val INST = insert(instRegValid ? instReg | iCache.io.fetchPorts.output.dout)

    iBus << iCache.io.ibus
  }

  val fetch3Action = new fetch3.Area {
    fetch3.throwWhen(flush, usingReady = true)

    fetch3.haltWhen(!output.ready)
    output.dout.pc := up(fetch1Action.PC)
    output.dout.inst := up(fetch2Action.INST)
    output.dout.predict := up(fetch1Action.PREDICT)
    output.dout.exDescription := up(fetch1Action.EX_DESCRIPTION)
    output.valid := up.valid
  }

  Builder(fetch1, fetch2, fetch3, fetch1To2, fetch2To3)
}

object FetchPipelineVerilog extends App {
  Config.spinal.generateVerilog(new FetchPipeline(
    cacheConfig = Config.iCacheConfig,
    bpuConfig = Config.bpuConfig,
    tlbConfig = Config.tlbConfig,
    fetchPortsNum = 2,
    _type = "sim"
  ))
}