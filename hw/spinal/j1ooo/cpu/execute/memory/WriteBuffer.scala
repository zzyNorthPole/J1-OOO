package j1ooo.cpu.execute.memory

import j1ooo.gen.{CacheConfig, Config, WriteBufferConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, PriorityMux, master, slave, traversableOnceBoolPimped}

case class WriteBufferItem(cacheConfig: CacheConfig) extends Bundle {
  val way = UInt(log2Up(cacheConfig.ways) bits)
  val wea = Bits(4 bits)
  val addr = UInt(32 bits)
  val data = UInt(32 bits)
}

case class WriteBufferPushBus(cacheConfig: CacheConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val payload = WriteBufferItem(cacheConfig)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready)
  }
}

case class WriteBufferPopBus(cacheConfig: CacheConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val payload = WriteBufferItem(cacheConfig)

  override def asMaster(): Unit = {
    out(valid, payload)
  }

  def << (that: WriteBufferPopBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
  }
}

class WriteBuffer(cacheConfig: CacheConfig, writeBufferConfig: WriteBufferConfig, commitPorts: Int) extends Component {
  val io = new Bundle {
    val empty = out Bool()
    val queryPorts = slave(StoreBufferQueryBus())
    val pushPorts = Vec(slave(WriteBufferPushBus(cacheConfig)), commitPorts)
    val popPorts = master(WriteBufferPopBus(cacheConfig))
  }
  noIoPrefix()

  import io._

  val front = RegInit(U(0, log2Up(writeBufferConfig.lines) bits))
  val end = RegInit(U(0, log2Up(writeBufferConfig.lines) bits))
  val buffer = Vec(Reg(WriteBufferItem(cacheConfig)), writeBufferConfig.lines)

  empty := front === end
  pushPorts(0).ready := (end + 1 =/= front)
  pushPorts(1).ready := (end + 1 =/= front) && (end + 2 =/= front)
  when(pushPorts(0).valid) {
    buffer(end) := pushPorts(0).payload
    end := end + 1
  }
  when(pushPorts(1).valid) {
    buffer(end + 1) := pushPorts(1).payload
    end := end + 2
  }

  popPorts.valid.clear()
  popPorts.payload := buffer(front)
  when(front =/= end) {
    popPorts.valid.set()
    front := front + 1
  }

  val hits, hitsAfterFront, hitsBeforeEnd, hitsBetweenFrontEnd = Vec(Bool(), writeBufferConfig.lines)
  val hitDatas = Vec(UInt(32 bits), writeBufferConfig.lines)
  for (i <- 0 until writeBufferConfig.lines) {
    hits(writeBufferConfig.lines - 1 - i) := (
      buffer(i).addr === queryPorts.addr && queryPorts.cached
    )
    hitsAfterFront(writeBufferConfig.lines - 1 - i) := (
      hits(writeBufferConfig.lines - 1 - i) && front <= U(i, log2Up(writeBufferConfig.lines) bits)
    )
    hitsBeforeEnd(writeBufferConfig.lines - 1 - i) := (
      hits(writeBufferConfig.lines - 1 - i) && end > U(i, log2Up(writeBufferConfig.lines) bits)
    )
    hitsBetweenFrontEnd(writeBufferConfig.lines - 1 - i) := (
      hits(writeBufferConfig.lines - 1 - i) &&
        front <= U(i, log2Up(writeBufferConfig.lines) bits) && end > U(i, log2Up(writeBufferConfig.lines) bits)
    )
    hitDatas(writeBufferConfig.lines - 1 - i) := buffer(i).data
  }
  when(front <= end) {
    queryPorts.hit := hitsBetweenFrontEnd.orR
    queryPorts.dout := PriorityMux(hitsBetweenFrontEnd, hitDatas)
  }.otherwise {
    queryPorts.hit := hitsBeforeEnd.orR | hitsAfterFront.orR
    queryPorts.dout := hitsBeforeEnd.orR ? PriorityMux(hitsBeforeEnd, hitDatas) | PriorityMux(hitsAfterFront, hitDatas)
  }
}

object WriteBufferVerilog extends App {
  Config.spinal.generateVerilog(new WriteBuffer(cacheConfig = Config.dCacheConfig, writeBufferConfig = Config.writeBufferConfig, commitPorts = 2))
}