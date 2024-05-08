package j1ooo.cpu.execute.memory

import j1ooo.cpu.execute.normal.BypassInputBus
import j1ooo.cpu.signal.LsuOp
import j1ooo.cpu.util.BusRename
import j1ooo.gen.Config.axi4Config
import j1ooo.gen.{CacheConfig, Config, LSTConfig, MSHRConfig, RobConfig, StoreBufferConfig}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.{Counter, IMasterSlave, master, slave}

case class MSHRItem(cacheConfig: CacheConfig) extends Bundle {
  val valid = Bool()
  // cached load/store: (putc) + getc + isWrite
  // uncached load: getu
  // uncached store: putu
  // cache operation: putc
  val getc, putc, getu, putu = Bool()
  val isWriteBack = Bool()
  // MSHR item address:
  // request cache line first address in cached load/store
  // replace cache line first address in cache operation
  // request data first address in uncached load/store
  val requestAddr = UInt(32 bits)
  val way = UInt(log2Up(cacheConfig.ways) bits)

  def init() = {
    valid init False
    this
  }
}

case class LoadStoreTableItem(cacheConfig: CacheConfig, mshrConfig: MSHRConfig, robConfig: RobConfig, storeBufferConfig: StoreBufferConfig) extends Bundle {
  val valid = Bool()
  val mshr = Vec(new Bundle {
    val valid = Bool()
    val index = UInt(log2Up(mshrConfig.lines) bits)
  }, 2)
  // isCacheD == 1 isWrite == 0 CACHE
  // isCacheD == 0 isWrite == 0 load
  // isCacheD == 0 isWrite == 1 store
  val isCacheD = Bool()
  val isWrite = Bool()
  val op = LsuOp()
  val offset = UInt(cacheConfig.offsetWidth bits)
  // LST item address:
  // rob index in cached/uncached load
  val robDestAddr = UInt(log2Up(robConfig.lines) bits)
  // store buffer item in cached/uncached store
  val storeBufferDestAddr = UInt(log2Up(storeBufferConfig.lines) bits)
  // val destAddr = UInt(log2Up(storeBufferConfig.lines) bits)
  val data = UInt(32 bits)

  def init() = {
    valid init False
    this
  }
}

case class MSHRPushBus(cacheConfig: CacheConfig, mshrConfig: MSHRConfig, robConfig: RobConfig, storeBufferConfig: StoreBufferConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val mshrItem = Vec(MSHRItem(cacheConfig), 2)
  val lstItem = LoadStoreTableItem(cacheConfig, mshrConfig, robConfig, storeBufferConfig)

  override def asMaster(): Unit = {
    in(ready)
    out(valid, mshrItem, lstItem)
  }

  def << (that: MSHRPushBus): Unit = {
    this.valid := that.valid
    that.ready := this.ready
    this.mshrItem := that.mshrItem
    this.lstItem := that.lstItem
  }
}

case class MSHRToDCacheBus(cacheConfig: CacheConfig, _type: String, use4Write: Boolean) extends Bundle with IMasterSlave {
  val valid = Bool()
  val way = UInt(log2Up(cacheConfig.ways) bits)
  val index = UInt((if (_type != "data") cacheConfig.indexWidth else cacheConfig.indexWidth + cacheConfig.offsetWidth - 2) bits)
  val payload = UInt((if (_type == "tag") cacheConfig.tagWidth else if (_type == "data") 32 else 1) bits)

  override def asMaster(): Unit = {
    (use4Write == true) generate out(valid, way, index, payload)
    (use4Write == false) generate {
      out(valid, way, index)
      in(payload)
    }
  }

  def << (that: MSHRToDCacheBus): Unit = {
    this.valid := that.valid
    this.way := that.way
    this.index := that.index
    (use4Write == true) generate {
      this.payload := that.payload
    }
    (use4Write == false) generate {
      that.payload := this.payload
    }
  }
}

case class MSHRToCommitBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val addr = UInt(log2Up(robConfig.lines) bits)
  val data = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid)
    in(ready, addr, data)
    // in(ready, data)
  }

  def << (that: MSHRToCommitBus): Unit = {
    this.valid := that.valid
    that.ready := this.ready
    that.addr := this.addr
    that.data := this.data
  }
}

case class MSHRToStoreBufferBus(storeBufferConfig: StoreBufferConfig, _type: String) extends Bundle with IMasterSlave {
  val valid = (_type == "write") generate Bool()
  val addr = UInt(log2Up(storeBufferConfig.lines) bits)
  val data = UInt(32 bits)

  override def asMaster(): Unit = {
    (_type == "write") generate out(valid, addr, data)
    (_type == "read") generate {
      out(addr)
      in(data)
    }
  }

  def << (that: MSHRToStoreBufferBus): Unit = {
    (_type == "write") generate {
      this.valid := that.valid
      this.data := that.data
    }
    this.addr := that.addr
    (_type == "read") generate {
      that.data := this.data
    }
  }
}

class MSHR(
          cacheConfig: CacheConfig,
          robConfig: RobConfig,
          storeBufferConfig: StoreBufferConfig,
          mshrConfig: MSHRConfig,
          lstConfig: LSTConfig
          ) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val pushPorts = slave(MSHRPushBus(cacheConfig, mshrConfig, robConfig, storeBufferConfig))

    val popPorts = new Bundle {
      val dCache = new Bundle {
        val fetchBus = master(MSHRToDCacheBus(cacheConfig, "data", false))
        val updateInfo = master(MSHRToDCacheBus(cacheConfig, "tag", true))
        val updateData = master(MSHRToDCacheBus(cacheConfig, "data", true))
        val removeStuck = master(MSHRToDCacheBus(cacheConfig, "removeStuck", true))
        val invalidate = master(MSHRToDCacheBus(cacheConfig, "invalidate", true))
        val refetch = out Bool()
      }
      val commit = slave(MSHRToCommitBus(robConfig))
      val bypass = master(BypassInputBus(robConfig))
      val storeBuffer = new Bundle {
        val read = master(MSHRToStoreBufferBus(storeBufferConfig, "read"))
        val write = master(MSHRToStoreBufferBus(storeBufferConfig, "write"))
        val pop = out Bool()
      }
      val writeBuffer = new Bundle {
        val empty = in Bool()
      }
      val writeQueue = new Bundle {
        val empty = in Bool()
        val full = in Bool()
        val resp = master(WriteQueueBus())
      }
    }

    val bus = master(Axi4(axi4Config)).setIdle()
  }
  noIoPrefix()
  BusRename.setName(io.bus)

  import io._

  val mshr = Vec(Reg(MSHRItem(cacheConfig)).init(), mshrConfig.lines)
  val lst = Vec(Reg(LoadStoreTableItem(cacheConfig, mshrConfig, robConfig, storeBufferConfig)).init(), lstConfig.lines)
  val replaceData = Vec(Reg(UInt(32 bits)), cacheConfig.words)
  // temp set item num to 1
  assert(mshrConfig.lines == 2)
  assert(lstConfig.lines == 1)

  when(flush) {
    for (i <- 0 until mshrConfig.lines) {
      mshr(i).valid.clear()
    }
    for (i <- 0 until lstConfig.lines) {
      lst(i).valid.clear()
    }
  }

  pushPorts.ready := !mshr(0).valid && !mshr(1).valid && !lst(0).valid
  when(pushPorts.valid && !flush) {
    mshr(0) := pushPorts.mshrItem(0)
    mshr(1) := pushPorts.mshrItem(1)
    lst(0) := pushPorts.lstItem
  }

  val getcIndex = UInt(log2Up(mshrConfig.lines) bits)
  getcIndex := mshr(1).valid ? U(1, log2Up(mshrConfig.lines) bits) | U(0, log2Up(mshrConfig.lines) bits)

  val count, countPre = Counter(cacheConfig.words)
  val busInit = new Area {
    import bus._

    aw.addr := mshr(0).requestAddr(31 downto cacheConfig.offsetWidth) @@ U(0, cacheConfig.offsetWidth bits)
    aw.id := U(1, 4 bits)
    aw.len := U(cacheConfig.words - 1, 8 bits)
    aw.size := U(2, 3 bits) // 4 bytes each time
    aw.burst := B(1, 2 bits) // INCR
    // aw.lock := B(0, 2 bits)
    aw.cache := B(0, 4 bits)
    aw.prot := B(0, 3 bits)

    w.data := replaceData(count).asBits
    w.strb := B((3 downto 0) -> True)
    w.last := (count === U(cacheConfig.words - 1, log2Up(cacheConfig.words) bits))

    ar.addr := (
      mshr(getcIndex).getc ?
        (mshr(getcIndex).requestAddr(31 downto cacheConfig.offsetWidth) @@ U(0, cacheConfig.offsetWidth bits)) |
        lst(0).op.mux(
          LsuOp.B -> mshr(0).requestAddr,
          LsuOp.BU -> mshr(0).requestAddr,
          LsuOp.H -> (mshr(0).requestAddr(31 downto 1) @@ U(0, 1 bits)),
          LsuOp.HU -> (mshr(0).requestAddr(31 downto 1) @@ U(0, 1 bits)),
          default -> (mshr(0).requestAddr(31 downto 2) @@ U(0, 2 bits))
        ) // TODO maybe physical address error
    )
    ar.id := U(1, 4 bits)
    ar.len := mshr(getcIndex).getc ? U(cacheConfig.words - 1, 8 bits) | U(0, 8 bits)
    ar.size := (
      mshr(getcIndex).getc ?
        U(2, 3 bits) /* 4 bytes */ |
        lst(0).op.mux(
          LsuOp.B -> U(0, 3 bits),
          LsuOp.BU -> U(0, 3 bits),
          LsuOp.H -> U(1, 3 bits),
          LsuOp.HU -> U(1, 3 bits),
          default -> U(2, 3 bits)
        )
    )
    ar.burst := B(1, 2 bits) /* INCR */
    // ar.lock := B(0, 2 bits)
    ar.cache := B(0, 4 bits)
    ar.prot := B(0, 3 bits)
  }

  popPorts.dCache.fetchBus.valid.clear() // init false
  popPorts.dCache.fetchBus.way := mshr(0).way
  popPorts.dCache.fetchBus.index := mshr(0).requestAddr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits) @@ count
  val fetchValid, fetchReady = CombInit(False)
  val fetchFSM = new StateMachine {
    setEntry(stateBoot)
    disableAutoStart()

    val preFetch = new State()
    val fetch = new State()

    stateBoot.whenIsActive {
      when(fetchValid) {
        count.clear()
        goto(preFetch)
      }
    }

    preFetch.whenIsActive {
      popPorts.dCache.fetchBus.valid.set()
      count.increment()
      countPre.clear()
      goto(fetch)
    }

    fetch.whenIsActive {
      when(countPre.willOverflow) {
        fetchReady.set()
        goto(stateBoot)
      }.otherwise {
        popPorts.dCache.fetchBus.valid.set()
      }
      count.increment()
      countPre.increment()
      replaceData(countPre) := popPorts.dCache.fetchBus.payload
    }
  }
  val writeBackValid, writeBackReady = CombInit(False)
  val writeBackFSM = new StateMachine {
    import bus._

    setEntry(stateBoot)
    disableAutoStart()

    val writeBackAw = new State()
    val writeBackW = new State()
    val writeBackB = new State()
    stateBoot.whenIsActive {
      when(writeBackValid) {
        goto(writeBackAw)
      }
    }

    writeBackAw.whenIsActive {
      aw.valid.set()
      when(aw.ready) {
        count.clear()
        goto(writeBackW)
      }
    }

    writeBackW.whenIsActive {
      w.valid.set()
      when(w.ready) {
        count.increment()
        when(w.last) {
          goto(writeBackB)
        }
      }
    }

    writeBackB.whenIsActive {
      b.ready.set()
      when(b.valid) {
        writeBackReady.set()
        goto(stateBoot)
      }
    }
  }
  popPorts.dCache.updateInfo.valid.clear() // init false
  popPorts.dCache.updateInfo.way := mshr(getcIndex).way
  popPorts.dCache.updateInfo.index := mshr(getcIndex).requestAddr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits)
  popPorts.dCache.updateInfo.payload := mshr(getcIndex).requestAddr(32 - cacheConfig.tagWidth, cacheConfig.tagWidth bits)
  popPorts.dCache.updateData.valid.clear() // init false
  popPorts.dCache.updateData.way := mshr(getcIndex).way
  popPorts.dCache.updateData.index := mshr(getcIndex).requestAddr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits) @@ count
  popPorts.dCache.updateData.payload := bus.r.data.asUInt
  val readNewValid, readNewReady = CombInit(False)
  val readNewFSM = new StateMachine {
    import bus._

    setEntry(stateBoot)
    disableAutoStart()

    val readNewAr = new State()
    val readNewR = new State()

    stateBoot.whenIsActive {
      when(readNewValid) {
        goto(readNewAr)
      }
    }

    readNewAr.whenIsActive {
      ar.valid.set()
      when(ar.ready) {
        count.clear()
        when(mshr(getcIndex).getc) {
          popPorts.dCache.updateInfo.valid.set()
        }
        goto(readNewR)
      }
    }

    readNewR.whenIsActive {
      r.ready.set()
      when(r.valid) {
        replaceData(count) := r.data.asUInt
        count.increment()
        when(mshr(getcIndex).getc) {
          popPorts.dCache.updateData.valid.set()
        }
        when(r.last) {
          readNewReady.set()
          goto(stateBoot)
        }
      }
    }
  }
  // dcache
  popPorts.dCache.removeStuck.valid.clear()
  popPorts.dCache.removeStuck.valid.setWhen(flush && (mshr(0).getc || mshr(0).putc))
  popPorts.dCache.removeStuck.way := mshr(getcIndex).way
  popPorts.dCache.removeStuck.index := mshr(getcIndex).requestAddr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits)
  popPorts.dCache.removeStuck.payload := U(0, 1 bits)
  popPorts.dCache.invalidate.valid.clear()
  popPorts.dCache.invalidate.way := mshr(0).way
  popPorts.dCache.invalidate.index := mshr(0).requestAddr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits)
  popPorts.dCache.invalidate.payload := U(0, 1 bits)
  popPorts.dCache.refetch.clear()
  // rob
  popPorts.commit.ready.clear()
  popPorts.commit.addr := lst(0).robDestAddr
  val selectedData = mshr(getcIndex).getu ? replaceData(0) | replaceData(lst(0).offset(2, cacheConfig.offsetWidth - 2 bits))
  popPorts.commit.data := lst(0).op.mux(
    LsuOp.B -> mshr(getcIndex).requestAddr(0, 2 bits).muxListDc(
      for (i <- 0 until 4) yield (i, B(24 bits, default -> selectedData(i * 8 + 7)).asUInt @@ selectedData(i * 8, 8 bits))
    ),
    LsuOp.BU -> mshr(getcIndex).requestAddr(0, 2 bits).muxListDc(
      for (i <- 0 until 4) yield (i, U(0, 24 bits) @@ selectedData(i * 8, 8 bits))
    ),
    LsuOp.H -> mshr(getcIndex).requestAddr(1, 1 bits).muxListDc(
      for (i <- 0 until 2) yield (i, B(16 bits, default -> selectedData(i * 16 + 15)).asUInt @@ selectedData(i * 16, 16 bits))
    ),
    LsuOp.HU -> mshr(getcIndex).requestAddr(1, 1 bits).muxListDc(
      for (i <- 0 until 2) yield (i, U(0, 16 bits) @@ selectedData(i * 16, 16 bits))
    ),
    LsuOp.W -> selectedData,
    LsuOp.WL -> mshr(getcIndex).requestAddr(0, 2 bits).muxListDc(
      for (i <- 0 until 4) yield (i, if (i == 3) selectedData else selectedData((i * 8 + 7) downto 0) @@ lst(0).data(((3 - i) * 8 - 1) downto 0))
    ),
    LsuOp.WR -> mshr(getcIndex).requestAddr(0, 2 bits).muxListDc(
      for (i <- 0 until 4) yield (i, if (i == 0) selectedData else lst(0).data(31 downto ((4 - i) * 8)) @@ selectedData(31 downto (i * 8)))
    )
  )
  // bypass
  popPorts.bypass.valid.clear()
  popPorts.bypass.addr := popPorts.commit.addr
  popPorts.bypass.data := popPorts.commit.data
  // store buffer
  popPorts.storeBuffer.read.addr := lst(0).storeBufferDestAddr
  /* popPorts.storeBuffer.read.data */
  popPorts.storeBuffer.write.valid.clear()
  popPorts.storeBuffer.write.addr := lst(0).storeBufferDestAddr
  popPorts.storeBuffer.write.data := lst(0).op.mux(
    LsuOp.B -> replaceData(lst(0).offset(2, cacheConfig.offsetWidth - 2 bits))(31 downto 8) @@ popPorts.storeBuffer.read.data(7 downto 0),
    LsuOp.BU -> replaceData(lst(0).offset(2, cacheConfig.offsetWidth - 2 bits))(31 downto 8) @@ popPorts.storeBuffer.read.data(7 downto 0),
    LsuOp.H -> replaceData(lst(0).offset(2, cacheConfig.offsetWidth - 2 bits))(31 downto 16) @@ popPorts.storeBuffer.read.data(15 downto 0),
    LsuOp.HU -> replaceData(lst(0).offset(2, cacheConfig.offsetWidth - 2 bits))(31 downto 16) @@ popPorts.storeBuffer.read.data(15 downto 0),
    default -> popPorts.storeBuffer.read.data
  )
  popPorts.storeBuffer.pop.clear()
  // write queue
  popPorts.writeQueue.resp.valid.clear()
  popPorts.writeQueue.resp.payload.op := lst(0).op
  popPorts.writeQueue.resp.payload.data := popPorts.storeBuffer.read.data.asBits
  popPorts.writeQueue.resp.payload.addr := mshr(0).requestAddr
  val commitFSM = new StateMachine {
    setEntry(stateBoot)
    disableAutoStart()

    val wait_ = new State()
    val fetch = new State()
    val putu = new State()
    val putc = new State()
    val get = new State()
    val broadcast = new State()
    // val rmstuck = new State()
    val invalid = new State()

    stateBoot.whenIsActive {
      when(popPorts.commit.valid) {
        goto(wait_)
      }
    }

    wait_.whenIsActive {
      when(mshr(0).putu) {
        when(!popPorts.writeQueue.full) {
          goto(putu)
        }
      }.otherwise {
        when(popPorts.writeBuffer.empty && popPorts.writeQueue.empty) {
          when(mshr(0).putc) {
            when(mshr(0).isWriteBack) {
              fetchValid.set()
              goto(fetch)
            }.otherwise {
              goto(invalid)
            }
          }.elsewhen(mshr(0).getc || mshr(0).getu) {
            readNewValid.set()
            goto(get)
          }
        }
      }
    }

    putu.whenIsActive {
      mshr(0).valid.clear()
      lst(0).valid.clear()
      popPorts.dCache.refetch.set()
      popPorts.storeBuffer.pop.set()
      popPorts.writeQueue.resp.valid.set()
      popPorts.commit.ready.set()
      goto(stateBoot)
    }

    fetch.whenIsActive {
      when(fetchReady) {
        writeBackValid.set()
        goto(putc)
      }
    }

    putc.whenIsActive {
      when(writeBackReady) {
        when(lst(0).mshr(1).valid) {
          readNewValid.set()
          goto(get)
        }.otherwise {
          goto(invalid)
        }
      }
    }

    get.whenIsActive {
      when(readNewReady) {
        goto(broadcast)
      }
    }

    broadcast.whenIsActive {
      // TODO
      popPorts.bypass.valid.set()
      for (i <- 0 until mshrConfig.lines) mshr(i).valid.clear()
      lst(0).valid.clear()
      popPorts.dCache.removeStuck.valid.setWhen(mshr(getcIndex).getc)
      popPorts.dCache.refetch.set()
      popPorts.storeBuffer.write.valid.setWhen(!lst(0).isCacheD && lst(0).isWrite)
      popPorts.commit.ready.set()
      goto(stateBoot)
    }

//    rmstuck.whenIsActive {
//      goto(stateBoot)
//    }

    invalid.whenIsActive {
      mshr(0).valid.clear()
      lst(0).valid.clear()
      popPorts.dCache.removeStuck.valid.set()
      popPorts.dCache.invalidate.valid.set()
      popPorts.dCache.refetch.set()
      popPorts.commit.ready.set()
      goto(stateBoot)
    }
  }
}

object MSHRVerilog extends App {
  Config.spinal.generateVerilog(new MSHR(Config.dCacheConfig, Config.robConfig, Config.storeBufferConfig, Config.mshrConfig, Config.lstConfig))
}