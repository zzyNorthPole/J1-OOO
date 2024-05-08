package j1ooo.cpu.execute.memory

import j1ooo.cpu.blackbox.RamParameter
import j1ooo.cpu.signal.{CacheOp, LsuOp}
import j1ooo.cpu.util.{Lfsr, Ram}
import j1ooo.gen.{CacheConfig, Config, MSHRConfig, RobConfig, StoreBufferConfig}
import spinal.core._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.{IMasterSlave, MuxOH, OHMasking, master, slave, traversableOnceBoolPimped}
import spinal.lib.misc.pipeline.{Builder, CtrlLink, StageLink}

case class DCacheInputBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val cached = Bool()
  val isWrite = Bool()
  val op = LsuOp()
  val addr = UInt(32 bits)
  val destAddr = UInt(log2Up(robConfig.lines) bits)
  val din = UInt(32 bits)
  val isCacheD = Bool()
  val cacheDOp = CacheOp()
  val exception = Bool()

  override def asMaster(): Unit = {
    out(valid, cached, isWrite, op, addr, destAddr, din, isCacheD, cacheDOp, exception)
  }
}

case class DCacheOutputBus() extends Bundle with IMasterSlave {
  val ready = Bool()
  val complete = Bool()
  val dout = UInt(32 bits)

  override def asMaster(): Unit = {
    out(ready, complete, dout)
  }
}

class DCache(
            cacheConfig: CacheConfig,
            mshrConfig: MSHRConfig,
            storeBufferConfig: StoreBufferConfig,
            robConfig: RobConfig,
            _type: String
            ) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val memPorts = new Bundle {
      val input = slave(DCacheInputBus(robConfig))
      val output = master(DCacheOutputBus())
    }

    val storeBufferQueryPorts = master(StoreBufferQueryBus())
    val storeBufferPushPorts = master(StoreBufferPushBus(cacheConfig, storeBufferConfig))
    val writeBufferQueryPorts = master(StoreBufferQueryBus())
    val writeBufferPopPorts = slave(WriteBufferPopBus(cacheConfig))

    // mshr
    val pushPorts = master(MSHRPushBus(cacheConfig, mshrConfig, robConfig, storeBufferConfig))

    val popPorts = new Bundle {
      val fetchBus = slave(MSHRToDCacheBus(cacheConfig, "data", false))
      val updateInfo = slave(MSHRToDCacheBus(cacheConfig, "tag", true))
      val updateData = slave(MSHRToDCacheBus(cacheConfig, "data", true))
      val removeStuck = slave(MSHRToDCacheBus(cacheConfig, "removeStuck", true))
      val invalidate = slave(MSHRToDCacheBus(cacheConfig, "invalidate", true))
      val refetch = in Bool()
    }
  }
  noIoPrefix()

  import io._

  val tagRams = Seq.fill(cacheConfig.ways) {
    Ram(_type, RamParameter(
      cacheConfig.lines, cacheConfig.tagWidth, false, "tdpram"
    ))
  }

  val validRams = Seq.fill(cacheConfig.ways) {
    Ram("vec", RamParameter(
      cacheConfig.lines, 1, false, "tdpram"
    ))
  }

  val dataRams = Seq.fill(cacheConfig.ways) {
    Ram(_type, RamParameter(
      cacheConfig.lines * cacheConfig.words, 32, true, "tdpram"
    ))
  }

  val dirtyRams = Seq.fill(cacheConfig.ways) {
    Ram("vec", RamParameter(
      cacheConfig.lines, 1, false, "tdpram"
    ))
  }

  val stuckRams = Seq.fill(cacheConfig.ways) {
    Ram("vec", RamParameter(
      cacheConfig.lines, 1, false, "tdpram"
    ))
  }

  val commitAction = new Area {

  }
  commitAction.setName("commit").reflectNames()

  val mem1, mem2 = CtrlLink()
  val mem1To2 = StageLink(mem1.down, mem2.up)

  val mem1Action = new mem1.Area {
    mem1.throwWhen(
      flush | memPorts.input.exception,
      usingReady = true
    )

    up.valid := memPorts.input.valid

    val CACHED = insert(io.memPorts.input.cached)

    val IS_WRITE = insert(io.memPorts.input.isWrite)
    val OP = insert(io.memPorts.input.op)

    val ADDR = insert(io.memPorts.input.addr)

    val TAG = insert(down(ADDR)(32 - cacheConfig.tagWidth, cacheConfig.tagWidth bits))
    val INDEX = insert(down(ADDR)(cacheConfig.offsetWidth, cacheConfig.indexWidth bits))
    val OFFSET = insert(down(ADDR)(0, cacheConfig.offsetWidth bits))

    val DEST_ADDR = insert(io.memPorts.input.destAddr)
    val curDin = (
      !memPorts.input.isWrite ? memPorts.input.din | memPorts.input.op.mux(
        LsuOp.B -> memPorts.input.din(0, 8 bits) @@ memPorts.input.din(0, 8 bits) @@ memPorts.input.din(0, 8 bits) @@ memPorts.input.din(0, 8 bits),
        LsuOp.H -> memPorts.input.din(0, 16 bits) @@ memPorts.input.din(0, 16 bits),
        LsuOp.WL -> memPorts.input.addr(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, U(0, 8 * (3 - i) bits) @@ memPorts.input.din(31 downto (8 * (3 - i))))
        ),
        LsuOp.WR -> memPorts.input.addr(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, memPorts.input.din(((4 - i) * 8 - 1) downto 0) @@ U(0, i * 8 bits))
        ),
        default -> memPorts.input.din
      )
    )
    val DIN = insert(curDin)

    val IS_CACHE_D = insert(io.memPorts.input.isCacheD)
    val CACHE_D_OP = insert(io.memPorts.input.cacheDOp)
  }
  mem1Action.setName("mem1").reflectNames()

  val mem2Action = new mem2.Area {
    mem2.throwWhen(flush)

    val tags = Vec(UInt(cacheConfig.tagWidth bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      tags(i) := tagRams(i).io.doutb
    }

    val valids = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      valids(i) := validRams(i).io.doutb.asBool
    }

    val datas = Vec(UInt(32 bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      datas(i) := dataRams(i).io.doutb
    }

    val dirtys = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      dirtys(i) := dirtyRams(i).io.doutb.asBool
    }

    val stucks = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      stucks(i) := stuckRams(i).io.doutb.asBool
    }

    val hits = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      hits(i) := (
        up.valid && up(mem1Action.CACHED) && !up(mem1Action.IS_CACHE_D) &&
          tags(i) === up(mem1Action.TAG) && valids(i) && !stucks(i)
      )
    }

    val hit = hits.orR
    val hitWay = MuxOH(
      hits,
      for (i <- 0 until cacheConfig.ways) yield U(i, log2Up(cacheConfig.ways) bits)
    )

    // invalid way select
    val invalids = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) invalids(i) := ~valids(i)
    val invalid = invalids.orR
    val invalidWay = MuxOH(
      OHMasking.first(invalids),
      for (i <- 0 until cacheConfig.ways) yield U(i, log2Up(cacheConfig.ways) bits)
    )

    // random replace way generation, sequential
    val lfsrWidth = log2Up(cacheConfig.ways) + 2
    val lfsr = new Lfsr(lfsrWidth)
    lfsr.io.en := up.isFiring
    lfsr.io.seed := U(lfsrWidth bits, default -> True)
    val lfsrDout = lfsr.io.dout

    // replace way, select by invalid condition
    val replaceWay = invalid ? invalidWay | lfsrDout(0, log2Up(cacheConfig.ways) bits)

    // cache operation
    val invalidateHits = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      invalidateHits(i) := (
        up.valid && up(mem1Action.CACHED) && up(mem1Action.IS_CACHE_D) && (up(mem1Action.CACHE_D_OP) =/= CacheOp.indexInvalidateWriteBack) &&
          ((tags(i) === up(mem1Action.TAG) && valids(i)) || stucks(i))
      )
    }
    val invalidateHit = invalidateHits.orR
    val hitInvalidateWay = MuxOH(
      invalidateHits,
      for (i <- 0 until cacheConfig.ways) yield U(i, log2Up(cacheConfig.ways) bits)
    )
    val indexInvalidateWay = up(mem1Action.TAG)(0, log2Up(cacheConfig.ways) bits)
    val invalidateByIndex = Bool()
    invalidateByIndex := (
      up.valid && up(mem1Action.CACHED) && up(mem1Action.IS_CACHE_D) && (up(mem1Action.CACHE_D_OP) === CacheOp.indexInvalidateWriteBack) &&
        (valids(indexInvalidateWay) || stucks(indexInvalidateWay))
    )
    val invalidateInvalid = up.valid && up(mem1Action.CACHED) && up(mem1Action.IS_CACHE_D) && !invalidateHit && !invalidateByIndex
    val invalidateWay = (up(mem1Action.CACHE_D_OP) === CacheOp.indexInvalidateWriteBack) ? indexInvalidateWay | hitInvalidateWay

    val targetWay = up(mem1Action.IS_CACHE_D) ? invalidateWay | (hit ? hitWay | replaceWay)

    storeBufferQueryPorts.cached := up(mem1Action.CACHED)
    storeBufferQueryPorts.addr := up(mem1Action.ADDR)
    writeBufferQueryPorts.cached := up(mem1Action.CACHED)
    writeBufferQueryPorts.addr := up(mem1Action.ADDR)
    val curData = storeBufferQueryPorts.hit ? storeBufferQueryPorts.dout | (writeBufferQueryPorts.hit ? writeBufferQueryPorts.dout | datas(targetWay))
    storeBufferPushPorts.valid := storeBufferPushPorts.ready && up.isFiring && up(mem1Action.IS_WRITE)
    storeBufferPushPorts.payload.isAvailable := hit
    storeBufferPushPorts.payload.cached := up(mem1Action.CACHED)
    storeBufferPushPorts.payload.op := up(mem1Action.OP)
    storeBufferPushPorts.payload.way := targetWay
    storeBufferPushPorts.payload.addr := up(mem1Action.ADDR)
    storeBufferPushPorts.payload.data := up(mem1Action.DIN)

    memPorts.output.ready.set()
    memPorts.output.complete.clear()
    memPorts.output.dout := up(mem1Action.OP).mux(
      LsuOp.B -> up(mem1Action.OFFSET)(0, 2 bits).muxListDc(
        for (i <- 0 until 4) yield (i, B(24 bits, default -> curData(i * 8 + 7)).asUInt @@ curData(i * 8, 8 bits))
      ),
      LsuOp.BU -> up(mem1Action.OFFSET)(0, 2 bits).muxListDc(
        for (i <- 0 until 4) yield (i, U(0, 24 bits) @@ curData(i * 8, 8 bits))
      ),
      LsuOp.H -> up(mem1Action.OFFSET)(1, 1 bits).muxListDc(
        for (i <- 0 until 2) yield (i, B(16 bits, default -> curData(i * 16 + 15)).asUInt @@ curData(i * 16, 16 bits))
      ),
      LsuOp.HU -> up(mem1Action.OFFSET)(1, 1 bits).muxListDc(
        for (i <- 0 until 2) yield (i, U(0, 16 bits) @@ curData(i * 16, 16 bits))
      ),
      LsuOp.W -> curData,
      LsuOp.WL -> up(mem1Action.OFFSET)(0, 2 bits).muxListDc(
        for (i <- 0 until 4) yield (i, if (i == 3) curData else curData((i * 8 + 7) downto 0) @@ up(mem1Action.DIN)(((3 - i) * 8 - 1) downto 0))
      ),
      LsuOp.WR -> up(mem1Action.OFFSET)(0, 2 bits).muxListDc(
        for (i <- 0 until 4) yield (i, if (i == 0) curData else up(mem1Action.DIN)(31 downto ((4 - i) * 8)) @@ curData(31 downto (i * 8)))
      )
    )
    pushPorts.valid.clear()
    // getc: cached load/store miss
    // putc: cached load/store miss/cacheop
    // getu: uncached load
    // putu: uncached store
    pushPorts.mshrItem(0).valid.set()
    pushPorts.mshrItem(0).getc := up(mem1Action.CACHED) && !up(mem1Action.IS_CACHE_D) && (!valids(targetWay) || !dirtys(targetWay))
    pushPorts.mshrItem(0).putc := up(mem1Action.CACHED) && (
      (!up(mem1Action.IS_CACHE_D) && valids(targetWay) && dirtys(targetWay)) || up(mem1Action.IS_CACHE_D)
    )
    pushPorts.mshrItem(0).getu := !up(mem1Action.CACHED) && !up(mem1Action.IS_WRITE)
    pushPorts.mshrItem(0).putu := !up(mem1Action.CACHED) && up(mem1Action.IS_WRITE)
    pushPorts.mshrItem(0).isWriteBack := up(mem1Action.CACHED) && dirtys(targetWay)
    pushPorts.mshrItem(0).requestAddr := pushPorts.mshrItem(0).putc ? (
      tags(targetWay) @@ up(mem1Action.INDEX) @@ up(mem1Action.OFFSET)
    ) | up(mem1Action.ADDR)
    pushPorts.mshrItem(0).way := targetWay
    pushPorts.mshrItem(1).valid := up(mem1Action.CACHED) && !up(mem1Action.IS_CACHE_D) && valids(targetWay) && dirtys(targetWay)
    pushPorts.mshrItem(1).getc := True
    pushPorts.mshrItem(1).putc := False
    pushPorts.mshrItem(1).getu := False
    pushPorts.mshrItem(1).putu := False
    pushPorts.mshrItem(1).isWriteBack := False
    pushPorts.mshrItem(1).requestAddr := up(mem1Action.ADDR)
    pushPorts.mshrItem(1).way := targetWay
    pushPorts.lstItem.valid.set()
    pushPorts.lstItem.mshr(0).valid := pushPorts.mshrItem(0).valid
    pushPorts.lstItem.mshr(0).index := U(0, log2Up(mshrConfig.lines) bits)
    pushPorts.lstItem.mshr(1).valid := pushPorts.mshrItem(1).valid
    pushPorts.lstItem.mshr(1).index := U(1, log2Up(mshrConfig.lines) bits)
    pushPorts.lstItem.isCacheD := up(mem1Action.IS_CACHE_D)
    pushPorts.lstItem.isWrite := up(mem1Action.IS_WRITE)
    pushPorts.lstItem.op := up(mem1Action.OP)
    pushPorts.lstItem.offset := up(mem1Action.OFFSET)
    pushPorts.lstItem.robDestAddr := up(mem1Action.DEST_ADDR)
    pushPorts.lstItem.storeBufferDestAddr := up(mem1Action.IS_WRITE) ? storeBufferPushPorts.destAddr | U(0, log2Up(storeBufferConfig.lines) bits)
    pushPorts.lstItem.data := up(mem1Action.DIN)

    val storeBufferNotReadyRefetch = CombInit(False)
    val waitFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val wait_ = new State()

      stateBoot.whenIsActive {
        when(up.valid && !flush) {
          when(hit || invalidateInvalid) {
            when(up(mem1Action.IS_WRITE) && !storeBufferPushPorts.ready) {
              storeBufferNotReadyRefetch.set()
              memPorts.output.ready.clear()
            }.otherwise {
              memPorts.output.ready.set()
              memPorts.output.complete.set()
            }
          }.otherwise {
            when(!popPorts.refetch) {
              when(pushPorts.ready) {
                pushPorts.valid.set()
                memPorts.output.ready.set()
                memPorts.output.complete.clear()
              }.otherwise {
                memPorts.output.ready.clear()
                goto(wait_)
              }
            }.otherwise {
              memPorts.output.ready.clear()
            }
          }
        }
      }

      wait_.whenIsActive {
        memPorts.output.ready.clear()
        when(flush || popPorts.refetch) {
          goto(stateBoot)
        }
      }
    }
    mem2.haltWhen(!memPorts.output.ready)
  }
  mem2Action.setName("mem2").reflectNames()
  Builder(mem1, mem2, mem1To2)
  val memRamPortsInit = new Area {
    for (i <- 0 until cacheConfig.ways) {
      val curRam = tagRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      curRam.io.ena := popPorts.updateInfo.valid && (curWay === popPorts.updateInfo.way)
      curRam.io.wea := B(1, 1 bits)
      curRam.io.addra := popPorts.updateInfo.index
      curRam.io.dina := popPorts.updateInfo.payload
      curRam.io.enb := True
      curRam.io.web := B(0, 1 bits)
      curRam.io.addrb := (
        ((popPorts.refetch && !mem2Action.up.isReady) || mem2Action.storeBufferNotReadyRefetch) ?
          mem2Action.up(mem1Action.INDEX) |
          mem1Action.down(mem1Action.INDEX)
      )
      curRam.io.dinb := U(0, cacheConfig.tagWidth bits)
    }

    for (i <- 0 until cacheConfig.ways) {
      val curRam = validRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      curRam.io.ena := (
        (popPorts.updateInfo.valid && curWay === popPorts.updateInfo.way) ||
          (popPorts.invalidate.valid && curWay === popPorts.invalidate.way)
      )
      curRam.io.wea := B(1, 1 bits)
      curRam.io.addra := (
        (popPorts.updateInfo.valid && curWay === popPorts.updateInfo.way) ?
          popPorts.updateInfo.index |
          popPorts.invalidate.index
      )
      curRam.io.dina := (
        (popPorts.updateInfo.valid && curWay === popPorts.updateInfo.way) ?
          U(1, 1 bits) | U(0, 1 bits)
      )
      curRam.io.enb := True
      curRam.io.web := B(0, 1 bits)
      curRam.io.addrb := (
        ((popPorts.refetch && !mem2Action.up.isReady) || mem2Action.storeBufferNotReadyRefetch) ?
          mem2Action.up(mem1Action.INDEX) |
          mem1Action.down(mem1Action.INDEX)
      )
      curRam.io.dinb := U(0, 1 bits)
    }

    val dataRamsDoutaValid = Vec(Bool(), cacheConfig.ways)
    val dataRamsDouta = Vec(UInt(32 bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      val curRam = dataRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      curRam.io.ena := (
        (popPorts.fetchBus.valid && curWay === popPorts.fetchBus.way) ||
          (popPorts.updateData.valid && curWay === popPorts.updateData.way) ||
          (writeBufferPopPorts.valid && curWay === writeBufferPopPorts.payload.way)
      )
      curRam.io.wea := MuxOH(
        Vec(
          writeBufferPopPorts.valid && curWay === writeBufferPopPorts.payload.way,
          popPorts.updateData.valid && curWay === popPorts.updateData.way,
          popPorts.fetchBus.valid && curWay === popPorts.fetchBus.way
        ),
        Vec(
          writeBufferPopPorts.payload.wea,
          B(4 bits, default -> True),
          B(0, 4 bits)
        )
      )
      curRam.io.addra := MuxOH(
        Vec(
          writeBufferPopPorts.valid && curWay === writeBufferPopPorts.payload.way,
          popPorts.updateData.valid && curWay === popPorts.updateData.way,
          popPorts.fetchBus.valid && curWay === popPorts.fetchBus.way
        ),
        Vec(
          writeBufferPopPorts.payload.addr(2, (cacheConfig.indexWidth + cacheConfig.offsetWidth - 2) bits),
          popPorts.updateData.index,
          popPorts.fetchBus.index
        )
      )
      curRam.io.dina := (
        (popPorts.updateData.valid && curWay === popPorts.updateData.way) ?
          popPorts.updateData.payload | writeBufferPopPorts.payload.data
      )
      dataRamsDoutaValid(i) := popPorts.fetchBus.valid && curWay === popPorts.fetchBus.way
      dataRamsDouta(i) := curRam.io.douta
      curRam.io.enb := True
      curRam.io.web := B(0, 4 bits)
      curRam.io.addrb := (
        ((popPorts.refetch && !mem2Action.up.isReady) || mem2Action.storeBufferNotReadyRefetch) ?
          (mem2Action.up(mem1Action.INDEX) @@ mem2Action.up(mem1Action.OFFSET)(2, (cacheConfig.offsetWidth - 2) bits)) |
          (mem1Action.down(mem1Action.INDEX) @@ mem1Action.down(mem1Action.OFFSET)(2, (cacheConfig.offsetWidth - 2) bits))
      )
      curRam.io.dinb := U(0, 32 bits)
    }
    popPorts.fetchBus.payload := MuxOH(dataRamsDoutaValid, dataRamsDouta)

    for (i <- 0 until cacheConfig.ways) {
      val curRam = dirtyRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      curRam.io.ena := (
        (popPorts.updateInfo.valid && curWay === popPorts.updateInfo.way) ||
          (writeBufferPopPorts.valid && curWay === writeBufferPopPorts.payload.way)
      )
      curRam.io.wea := B(1, 1 bits)
      curRam.io.addra := (
        (popPorts.updateInfo.valid && curWay === popPorts.updateInfo.way) ?
          popPorts.updateInfo.index | writeBufferPopPorts.payload.addr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits)
      )
      curRam.io.dina := (
        (popPorts.updateInfo.valid && curWay === popPorts.updateInfo.way) ?
          U(0, 1 bits) | U(1, 1 bits)
        )
      curRam.io.enb := True
      curRam.io.web := B(0, 1 bits)
      curRam.io.addrb := (
        ((popPorts.refetch && !mem2Action.up.isReady) || mem2Action.storeBufferNotReadyRefetch) ?
          mem2Action.up(mem1Action.INDEX) |
          mem1Action.down(mem1Action.INDEX)
      )
      curRam.io.dinb := U(0, 1 bits)
    }

    for (i <- 0 until cacheConfig.ways) {
      val curRam = stuckRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      curRam.io.ena := (
        (popPorts.removeStuck.valid && curWay === popPorts.removeStuck.way) ||
          (pushPorts.valid && mem2.up(mem1Action.CACHED) && curWay === pushPorts.mshrItem(0).way)
      )
      curRam.io.wea := B(1, 1 bits)
      curRam.io.addra := (
        (popPorts.removeStuck.valid && curWay === popPorts.removeStuck.way) ?
          popPorts.removeStuck.index | mem2Action.up(mem1Action.INDEX)
      )
      curRam.io.dina := (
        (popPorts.removeStuck.valid && curWay === popPorts.removeStuck.way) ?
          U(0, 1 bits) | U(1, 1 bits)
        )
      curRam.io.enb := True
      curRam.io.web := B(0, 1 bits)
      curRam.io.addrb := (
        ((popPorts.refetch && !mem2Action.up.isReady) || mem2Action.storeBufferNotReadyRefetch) ?
          mem2Action.up(mem1Action.INDEX) |
          mem1Action.down(mem1Action.INDEX)
      )
      curRam.io.dinb := U(0, 1 bits)
    }
  }
}

object DCacheVerilog extends App {
  Config.spinal.generateVerilog(new DCache(cacheConfig = Config.dCacheConfig, mshrConfig = Config.mshrConfig, storeBufferConfig = Config.storeBufferConfig, robConfig = Config.robConfig, _type = "sim"))
}