package j1ooo.cpu.execute.memory

import j1ooo.cpu.execute.RobMemoryWriteBackBus
import j1ooo.cpu.execute.normal.BypassInputBus
import j1ooo.cpu.fetch.FetchException
import j1ooo.cpu.mmu.{Cp0ToMmuBus, Mmu, TLBQueryBus}
import j1ooo.cpu.signal.{Exception, LsuOp}
import j1ooo.gen.Config.{axi4Config, dCacheConfig}
import j1ooo.gen.{CacheConfig, Config, LSTConfig, MSHRConfig, RobConfig, StoreBufferConfig, TlbConfig, WriteBufferConfig, WriteQueueConfig}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4WriteOnly}
import spinal.lib.misc.pipeline.{Builder, CtrlLink, StageLink}
import spinal.lib.{PriorityMux, master, slave}

class MemoryPipeline(
                    tlbConfig: TlbConfig,
                    cacheConfig: CacheConfig,
                    mshrConfig: MSHRConfig,
                    lstConfig: LSTConfig,
                    storeBufferConfig: StoreBufferConfig,
                    writeBufferConfig: WriteBufferConfig,
                    writeQueueConfig: WriteQueueConfig,
                    robConfig: RobConfig,
                    _type: String,
                    commitPorts: Int
                    ) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val inputPorts = slave(MemoryRsOutputBus(robConfig))
    val bypassPorts = Vec(master(BypassInputBus(robConfig)), 3)
    val outputPorts = master(RobMemoryWriteBackBus(robConfig))

    val cp0ToMmuBus = slave(Cp0ToMmuBus())
    val mmuToTlbBus = master(TLBQueryBus(tlbConfig))

    val commitToMshr = slave(MSHRToCommitBus(robConfig))
    val commitToCachedStore = new Bundle {
      val valid = in Vec(Bool(), commitPorts)
      val ready = out Bool()
    }
    val dbus = master(Axi4(axi4Config))
    val udbus = master(Axi4WriteOnly(axi4Config))
  }
  noIoPrefix()

  import io._

  val dCache = new DCache(cacheConfig, mshrConfig, storeBufferConfig, robConfig, _type)
  val mshr = new MSHR(cacheConfig, robConfig, storeBufferConfig, mshrConfig, lstConfig)
  val storeBuffer = new StoreBuffer(cacheConfig, storeBufferConfig, commitPorts)
  val writeBuffer = new WriteBuffer(cacheConfig, writeBufferConfig, commitPorts)
  val writeQueue = new WriteQueue(writeQueueConfig, axi4Config)

  val issue, mem1, mem2, writeBack = CtrlLink()
  val issueToMem1 = StageLink(issue.down, mem1.up)
  val mem1ToMem2 = StageLink(mem1.down, mem2.up)
  val mem2ToWriteBack = StageLink(mem2.down, writeBack.up)

  val issueAction = new issue.Area {
    issue.throwWhen(flush, usingReady = true)

    up.valid := inputPorts.valid

    inputPorts.ready := up.isReady

    val DIN0 = insert(inputPorts.payload.src(0).data)
    val DIN1 = insert(inputPorts.payload.src(1).data)

    val PC = insert(inputPorts.payload.pc)
    val OFFSET = insert(inputPorts.payload.offset)

    val DEST_VALID = insert(inputPorts.payload.dest.valid)
    val DEST_ADDR = insert(inputPorts.payload.dest.addr)

    val CTRL_AGU = insert(inputPorts.payload.ctrl.lsu)
    val CTRL_CACHE_OP = insert(inputPorts.payload.ctrl.cacheOp)
  }

  val mem1Action = new mem1.Area {
    mem1.throwWhen(flush, usingReady = true)

    val agu = new Agu()

    agu.io.input.ctrl := up(issueAction.CTRL_AGU)
    agu.io.input.din(0) := up(issueAction.DIN0)
    agu.io.input.din(1) := up(issueAction.OFFSET)

    val ADDR = insert(agu.io.addr)
    val addressErrorLoad = agu.io.addressErrorLoad
    val addressErrorStore = agu.io.addressErroeStore

    val mmu = new Mmu("D", tlbConfig, 1)
    mmu.io.pipeToMmu.valid := up.isValid && up(issueAction.CTRL_AGU).valid
    mmu.io.pipeToMmu.isWrite(0) := up(issueAction.CTRL_AGU).isWrite
    mmu.io.pipeToMmu.virtAddr(0) := agu.io.addr
    mmu.io.cp0ToMmuBus << cp0ToMmuBus
    mmuToTlbBus << mmu.io.mmuToTlbBus(0)
    val dTlbException = mmu.io.tlbException
    val exDescription = FetchException()
    exDescription.valid := addressErrorLoad || addressErrorStore || dTlbException(0).refill || dTlbException(0).invalid || dTlbException(0).modified
    exDescription.op := PriorityMux(
      Vec(
        addressErrorLoad,
        addressErrorStore,
        dTlbException(0).refill | dTlbException(0).invalid,
        dTlbException(0).modified
      ),
      Vec(
        Exception.ADEL(),
        Exception.ADES(),
        up(issueAction.CTRL_AGU).isWrite ? Exception.TLBS() | Exception.TLBL(),
        Exception.MOD()
      )
    )
    exDescription.isTlbHit := dTlbException(0).invalid
    val EX_DESCRIPTION = insert(exDescription)

    dCache.io.memPorts.input.valid := up.isValid && !up(issueAction.CTRL_CACHE_OP).validI
    dCache.io.memPorts.input.cached := mmu.io.mmuToCacheBus.cached(0)
    val IS_CACHED = insert(mmu.io.mmuToCacheBus.cached(0))
    dCache.io.memPorts.input.isWrite := up(issueAction.CTRL_AGU).isWrite
    dCache.io.memPorts.input.op := up(issueAction.CTRL_AGU).op
    dCache.io.memPorts.input.addr := mmu.io.mmuToCacheBus.phyAddr(0)
    dCache.io.memPorts.input.destAddr := up(issueAction.DEST_ADDR)
    dCache.io.memPorts.input.din := up(issueAction.DIN1)
    dCache.io.memPorts.input.isCacheD := up(issueAction.CTRL_CACHE_OP).validD
    dCache.io.memPorts.input.cacheDOp := up(issueAction.CTRL_CACHE_OP).op
    dCache.io.memPorts.input.exception := exDescription.valid
  }

  val mem2Action = new mem2.Area {
    mem2.throwWhen(flush, usingReady = true)

    mem2.haltWhen(!dCache.io.memPorts.output.ready)
    val COMPLETE = insert(dCache.io.memPorts.output.complete)
    val DOUT = insert(dCache.io.memPorts.output.dout)

    mshr.io.pushPorts << dCache.io.pushPorts

    storeBuffer.io.queryPorts << dCache.io.storeBufferQueryPorts

    writeBuffer.io.queryPorts << dCache.io.writeBufferQueryPorts

    storeBuffer.io.pushPorts << dCache.io.storeBufferPushPorts

    bypassPorts(0).valid := up.isFiring && down(COMPLETE) && !up(mem1Action.EX_DESCRIPTION).valid && up(issueAction.DEST_VALID)
    bypassPorts(0).addr := up(issueAction.DEST_ADDR)
    bypassPorts(0).data := down(DOUT)
  }

  val writeBackAction = new writeBack.Area {
    writeBack.throwWhen(flush, usingReady = true)

    val pc = up(issueAction.PC)
    outputPorts.valid := up.isFiring
    outputPorts.addr := up(issueAction.DEST_ADDR)
    outputPorts.payload.exception := up(mem1Action.EX_DESCRIPTION)
    outputPorts.payload.addr := up(mem1Action.ADDR)
    outputPorts.complete := up(mem2Action.COMPLETE)
    outputPorts.payload.isCached := up(mem1Action.IS_CACHED)
    outputPorts.dout := up(mem2Action.DOUT)

    bypassPorts(1).valid := up.isFiring && up(mem2Action.COMPLETE) && !up(mem1Action.EX_DESCRIPTION).valid && up(issueAction.DEST_VALID)
    bypassPorts(1).addr := up(issueAction.DEST_ADDR)
    bypassPorts(1).data := up(mem2Action.DOUT)
  }
  Builder(issue, mem1, mem2, writeBack, issueToMem1, mem1ToMem2, mem2ToWriteBack)

  val commitArea = new Area {
    dCache.io.flush := flush
    // async
    dCache.io.writeBufferPopPorts << writeBuffer.io.popPorts

    mshr.io.flush := flush

    mshr.io.popPorts.commit << commitToMshr

    bypassPorts(2) := mshr.io.popPorts.bypass

    dCache.io.popPorts.fetchBus << mshr.io.popPorts.dCache.fetchBus
    dCache.io.popPorts.updateInfo << mshr.io.popPorts.dCache.updateInfo
    dCache.io.popPorts.updateData << mshr.io.popPorts.dCache.updateData
    dCache.io.popPorts.removeStuck << mshr.io.popPorts.dCache.removeStuck
    dCache.io.popPorts.invalidate << mshr.io.popPorts.dCache.invalidate
    dCache.io.popPorts.refetch := mshr.io.popPorts.dCache.refetch

    storeBuffer.io.readPorts << mshr.io.popPorts.storeBuffer.read
    storeBuffer.io.writePorts << mshr.io.popPorts.storeBuffer.write
    storeBuffer.io.popUncached := mshr.io.popPorts.storeBuffer.pop

    mshr.io.popPorts.writeBuffer.empty := writeBuffer.io.empty

    mshr.io.popPorts.writeQueue.empty := writeQueue.io.empty
    mshr.io.popPorts.writeQueue.full := writeQueue.io.full
    writeQueue.io.input << mshr.io.popPorts.writeQueue.resp

    dbus.ar << mshr.io.bus.ar
    dbus.r >> mshr.io.bus.r
    dbus.aw << mshr.io.bus.aw
    dbus.w << mshr.io.bus.w
    dbus.b >> mshr.io.bus.b

    storeBuffer.io.flush := flush
    commitToCachedStore.ready.clear()
    storeBuffer.io.popPorts(0).valid.clear()
    storeBuffer.io.popPorts(1).valid.clear()
    writeBuffer.io.pushPorts(0).valid.clear()
    writeBuffer.io.pushPorts(1).valid.clear()
    when(commitToCachedStore.valid(0) && commitToCachedStore.valid(1)) {
      when(writeBuffer.io.pushPorts(0).ready && writeBuffer.io.pushPorts(1).ready) {
        commitToCachedStore.ready.set()
        storeBuffer.io.popPorts(0).valid.set()
        storeBuffer.io.popPorts(1).valid.set()
        writeBuffer.io.pushPorts(0).valid.set()
        writeBuffer.io.pushPorts(1).valid.set()
      }
    }.elsewhen(commitToCachedStore.valid(0)) {
      when(writeBuffer.io.pushPorts(0).ready) {
        commitToCachedStore.ready.set()
        storeBuffer.io.popPorts(0).valid.set()
        writeBuffer.io.pushPorts(0).valid.set()
      }
    }
    for (i <- 0 until commitPorts) {
      writeBuffer.io.pushPorts(i).payload.way := storeBuffer.io.popPorts(i).payload.way
      writeBuffer.io.pushPorts(i).payload.wea := storeBuffer.io.popPorts(i).payload.op.mux(
        LsuOp.B -> storeBuffer.io.popPorts(i).payload.addr(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, B(4 bits, i -> True, default -> False))
        ),
        LsuOp.H -> (storeBuffer.io.popPorts(i).payload.addr(1) ? B"1100" | B"0011"),
        LsuOp.W -> B"1111",
        LsuOp.WL -> storeBuffer.io.popPorts(i).payload.addr(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, B(4 bits, (i downto 0) -> True, default -> False))
        ),
        LsuOp.WR -> storeBuffer.io.popPorts(i).payload.addr(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, B(4 bits, (3 downto i) -> True, default -> False))
        ),
        default -> B"0000"
      )
      writeBuffer.io.pushPorts(i).payload.addr := storeBuffer.io.popPorts(i).payload.addr
      writeBuffer.io.pushPorts(i).payload.data := storeBuffer.io.popPorts(i).payload.data
    }

    udbus << writeQueue.io.udbusWrite
  }
}

object MemoryPipelineVerilog extends App {
  Config.spinal.generateVerilog(new MemoryPipeline(
    tlbConfig = Config.tlbConfig,
    cacheConfig = Config.dCacheConfig,
    mshrConfig = Config.mshrConfig,
    lstConfig = Config.lstConfig,
    storeBufferConfig = Config.storeBufferConfig,
    writeBufferConfig = Config.writeBufferConfig,
    writeQueueConfig = Config.writeQueueConfig,
    robConfig = Config.robConfig,
    _type = "sim",
    commitPorts = 2
  ))
}