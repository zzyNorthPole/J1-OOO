package j1ooo.cpu.mmu

import j1ooo.gen.{Config, TlbConfig}
import spinal.core._
import spinal.lib._
import spinal.lib.{IMasterSlave, slave}

// mips32 release 1 only support 4kb page
case class TLBVirt() extends Bundle {
  val VPN2 = UInt(19 bits)
  val ASID = UInt(8 bits)

  def init() = {
    VPN2 init 0
    ASID init 0
    this
  }
}

case class TLBPhy() extends Bundle {
  val PFN = UInt(20 bits)
  val C = UInt(3 bits)
  val D = Bool()
  val V = Bool()

  def init() = {
    PFN init 0
    C init 0
    D init False
    V init False
    this
  }

  def combine() = {
    PFN @@ C @@ D.asUInt @@ V.asUInt
  }
}

case class TLBItem() extends Bundle {
  val Virt = TLBVirt()
  val G = Bool()
  val Phy = Vec(TLBPhy(), 2)

  def init(): Unit = {
    Virt.VPN2 init U(19 bits, default -> True)
    Virt.ASID init U(0, 8 bits)
    G init False
    for (i <- 0 until 2) {
      Phy(i).PFN init U(0, 20 bits)
      Phy(i).C init U(0, 3 bits)
      Phy(i).D init False
      Phy(i).V init False
    }
  }
}

case class TLBWriteBus(tlbConfig: TlbConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(log2Up(tlbConfig.lines) bits)
  val din = TLBItem()

  override def asMaster(): Unit = {
    out(valid)
    out(addr)
    out(din)
  }

  def << (that: TLBWriteBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
    this.din := that.din
  }
}

case class TLBReadBus(tlbConfig: TlbConfig) extends Bundle with IMasterSlave {
  val addr = UInt(log2Up(tlbConfig.lines) bits)
  val dout = TLBItem()

  override def asMaster(): Unit = {
    out(addr)
    in(dout)
  }

  def << (that: TLBReadBus): Unit = {
    this.addr := that.addr
    that.dout := this.dout
  }
}

case class TLBReq() extends Bundle {
  val base = TLBVirt()
  val bias = Bool()
}

case class TLBQueryBus(tlbConfig: TlbConfig) extends Bundle with IMasterSlave {
  val din = TLBReq()
  val hit = Bool()
  val dout = TLBPhy()

  override def asMaster(): Unit = {
    out(din)
    in(hit, dout)
  }

  def << (that: TLBQueryBus): Unit = {
    this.din := that.din
    that.hit := this.hit
    that.dout := this.dout
  }
}

case class TLBPBus(tlbConfig: TlbConfig) extends Bundle with IMasterSlave {
  val din = TLBVirt()
  val hit = Bool()
  val addr = UInt(log2Up(tlbConfig.lines) bits)

  override def asMaster(): Unit = {
    out(din)
    in(hit, addr)
  }

  def << (that: TLBPBus): Unit = {
    this.din := that.din
    that.hit := this.hit
    that.addr := this.addr
  }
}

class TLB(tlbConfig: TlbConfig) extends Component {
  import tlbConfig._
  val io = new Bundle {
    val wPorts = slave(TLBWriteBus(tlbConfig))
    val rPorts = slave(TLBReadBus(tlbConfig))
    val queryPorts = Vec(slave(TLBQueryBus(tlbConfig)), ports)
    val tlbpPorts = slave(TLBPBus(tlbConfig))
  }
  noIoPrefix()

  import io._
  val tlb = Vec(Reg(TLBItem()), lines)
  for (i <- 0 until lines) {
    tlb(i).init()
  }

  val hits = Vec(Vec(Bool(), lines), ports)
  for (i <- 0 until ports) {
    for (j <- 0 until lines) {
      hits(i)(j) := (tlb(j).Virt.VPN2 === queryPorts(i).din.base.VPN2) && ((tlb(j).Virt.ASID === queryPorts(i).din.base.ASID) || tlb(j).G)
    }
    queryPorts(i).hit := hits(i).orR
  }
  for (i <- 0 until ports) {
    queryPorts(i).dout := MuxOH(
      hits(i),
      for (j <- 0 until lines) yield queryPorts(i).din.bias ? tlb(j).Phy(1) | tlb(j).Phy(0)
    )
  }

  val tlbpHits = Vec(Bool(), lines)
  for (i <- 0 until lines) {
    tlbpHits(i) := (tlb(i).Virt.VPN2 === tlbpPorts.din.VPN2) && ((tlb(i).Virt.ASID === tlbpPorts.din.ASID) || tlb(i).G)
  }
  tlbpPorts.hit := tlbpHits.orR
  tlbpPorts.addr := MuxOH(
    tlbpHits,
    for (i <- 0 until lines) yield U(i, log2Up(lines) bits)
  )

  when(wPorts.valid) {
    tlb(wPorts.addr) := wPorts.din
  }

  rPorts.dout := tlb(rPorts.addr)
}

object TLBVerilog extends App {
  Config.spinal.generateVerilog(new TLB(TlbConfig(3, 8)))
}