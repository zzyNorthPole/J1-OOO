package j1ooo.cpu.execute.memory

import j1ooo.cpu.signal.LsuOp
import j1ooo.gen.{CacheConfig, Config, StoreBufferConfig}
import spinal.core._
import spinal.lib.{Counter, IMasterSlave, PriorityMux, master, slave, traversableOnceBoolPimped}

case class StoreBufferItem(cacheConfig: CacheConfig) extends Bundle {
  val isAvailable = Bool()
  val cached = Bool()
  val op = LsuOp()
  val way = UInt(log2Up(cacheConfig.ways) bits)
  val addr = UInt(32 bits)
  // uncached store: din
  // cached store but miss: din
  // cached store and hit: dataRamRes + din
  val data = UInt(32 bits)
}

case class StoreBufferQueryBus() extends Bundle with IMasterSlave {
  val cached = Bool()
  val addr = UInt(32 bits)
  val hit = Bool()
  val dout = UInt(32 bits)

  override def asMaster(): Unit = {
    out(cached, addr)
    in(hit, dout)
  }

  def << (that: StoreBufferQueryBus): Unit = {
    this.cached := that.cached
    this.addr := that.addr
    that.hit := this.hit
    that.dout := this.dout
  }
}

case class StoreBufferPushBus(cacheConfig: CacheConfig, storeBufferConfig: StoreBufferConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val destAddr = UInt(log2Up(storeBufferConfig.lines) bits)
  val payload = StoreBufferItem(cacheConfig)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready, destAddr)
  }

  def << (that: StoreBufferPushBus): Unit = {
    this.valid := that.valid
    that.ready := this.ready
    that.destAddr := this.destAddr
    this.payload := that.payload
  }
}

case class StoreBufferPopBus(cacheConfig: CacheConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val payload = StoreBufferItem(cacheConfig)

  override def asMaster(): Unit = {
    out(valid)
    in(payload)
  }
}

class StoreBuffer(cacheConfig: CacheConfig, storeBufferConfig: StoreBufferConfig, commitPorts: Int) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val queryPorts = slave(StoreBufferQueryBus())

    val pushPorts = slave(StoreBufferPushBus(cacheConfig, storeBufferConfig))

    val popPorts = Vec(slave(StoreBufferPopBus(cacheConfig)), commitPorts)

    val readPorts = slave(MSHRToStoreBufferBus(storeBufferConfig, "read"))

    val writePorts = slave(MSHRToStoreBufferBus(storeBufferConfig, "write"))

    val popUncached = in Bool()
  }
  noIoPrefix()

  import io._

  val front = RegInit(U(0, log2Up(storeBufferConfig.lines) bits))
  val end = RegInit(U(0, log2Up(storeBufferConfig.lines) bits))
  val buffer = Vec(Reg(StoreBufferItem(cacheConfig)), storeBufferConfig.lines)

  when(flush) {
    front := 0
    end := 0
  }.otherwise {
    when(pushPorts.valid) {
      end := end + 1
      buffer(end) := pushPorts.payload
    }
    when(popPorts(0).valid && popPorts(1).valid) {
      front := front + 2
    }.elsewhen(popPorts(0).valid) {
      front := front + 1
    }
    when(popUncached) {
      front := front + 1
    }
  }
  pushPorts.ready := (end + 1) =/= front
  pushPorts.destAddr := end
  popPorts(0).payload := buffer(front)
  popPorts(1).payload := buffer(front + 1)

  val hits, hitsAfterFront, hitsBeforeEnd, hitsBetweenFrontEnd = Vec(Bool(), storeBufferConfig.lines)
  val hitDatas = Vec(UInt(32 bits), storeBufferConfig.lines)
  for (i <- 0 until storeBufferConfig.lines) {
    hits(storeBufferConfig.lines - 1 - i) := (
      buffer(i).isAvailable && buffer(i).cached &&
        buffer(i).addr === queryPorts.addr && queryPorts.cached
    )
    hitsAfterFront(storeBufferConfig.lines - 1 - i) := (
      hits(storeBufferConfig.lines - 1 - i) && front <= U(i, log2Up(storeBufferConfig.lines) bits)
    )
    hitsBeforeEnd(storeBufferConfig.lines - 1 - i) := (
      hits(storeBufferConfig.lines - 1 - i) && end > U(i, log2Up(storeBufferConfig.lines) bits)
    )
    hitsBetweenFrontEnd(storeBufferConfig.lines - 1 - i) := (
      hits(storeBufferConfig.lines - 1 - i) &&
        front <= U(i, log2Up(storeBufferConfig.lines) bits) && end > U(i, log2Up(storeBufferConfig.lines) bits)
    )
    hitDatas(storeBufferConfig.lines - 1 - i) := buffer(i).data
  }
  when(front <= end) {
    queryPorts.hit := hitsBetweenFrontEnd.orR
    queryPorts.dout := PriorityMux(hitsBetweenFrontEnd, hitDatas)
  }.otherwise {
    queryPorts.hit := hitsBeforeEnd.orR | hitsAfterFront.orR
    queryPorts.dout := hitsBeforeEnd.orR ? PriorityMux(hitsBeforeEnd, hitDatas) | PriorityMux(hitsAfterFront, hitDatas)
  }

  readPorts.data := buffer(readPorts.addr).data
  when(writePorts.valid) {
    buffer(writePorts.addr).isAvailable := True
    buffer(writePorts.addr).data := buffer(writePorts.addr).op.mux(
      LsuOp.B -> writePorts.data(0, 8 bits) @@ writePorts.data(0, 8 bits) @@ writePorts.data(0, 8 bits) @@ writePorts.data(0, 8 bits),
      LsuOp.H -> writePorts.data(0, 16 bits) @@ writePorts.data(0, 16 bits),
      LsuOp.WL -> buffer(writePorts.addr).addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, U(0, 8 * (3 - i) bits) @@ writePorts.data(31 downto (8 * (3 - i))))
      ),
      LsuOp.WR -> buffer(writePorts.addr).addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, writePorts.data(((4 - i) * 8 - 1) downto 0) @@ U(0, i * 8 bits))
      ),
      default -> writePorts.data
    )
  }
}

object StoreBufferVerilog extends App {
  Config.spinal.generateVerilog(new StoreBuffer(cacheConfig = Config.dCacheConfig, storeBufferConfig = Config.storeBufferConfig, commitPorts = 2))
}