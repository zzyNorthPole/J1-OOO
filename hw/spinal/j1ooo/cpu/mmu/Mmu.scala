package j1ooo.cpu.mmu

import j1ooo.gen.{Config, TlbConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, MuxOH, master, slave}

case class Cp0ToMmuBus() extends Bundle with IMasterSlave {
  val k0Cached = Bool()
  val ASID = UInt(8 bits)
  override def asMaster(): Unit = {
    out(k0Cached, ASID)
  }

  def << (that: Cp0ToMmuBus): Unit = {
    this.k0Cached := that.k0Cached
    this.ASID := that.ASID
  }
}

case class MmuToCacheBus(ports: Int) extends Bundle with IMasterSlave {
  val phyAddr = Vec(UInt(32 bits), ports)
  val cached = Vec(Bool(), ports)

  override def asMaster(): Unit = {
    out(phyAddr, cached)
  }
}

case class PipeToMmu(ports: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val isWrite = Vec(Bool(), ports)
  val virtAddr = Vec(UInt(32 bits), ports)

  override def asMaster(): Unit = {
    out(valid, isWrite, virtAddr)
  }
}

class Mmu(_type: String, tlbConfig: TlbConfig, ports: Int) extends Component {
  val io = new Bundle {
    // pipeline
    val pipeToMmu = slave(PipeToMmu(ports))

    // cp0
    val cp0ToMmuBus = slave(Cp0ToMmuBus())

    // cache
    val mmuToCacheBus = master(MmuToCacheBus(ports))

    // tlb
    val mmuToTlbBus = Vec(master(TLBQueryBus(tlbConfig)), ports)

    val tlbException = Vec(
      new Bundle {
        val refill = out Bool()
        val invalid = out Bool()
        val modified = (_type == "D") generate (out Bool())
      }, ports
    )
  }
  noIoPrefix()

  import io._

  val virtual = new Bundle {
    val useg = U"32'h00000000" // user mapped
    val kseg0 = U"32'h80000000" // kernel unmapped
    val kseg1 = U"32'hA0000000" // kernel unmapped uncached
    val ksseg = U"32'hC0000000" // supervisor mapped
    val kseg3 = U"32'hE0000000" // kernel mapped
  }
  val physical = new Bundle {
    val useg = U"32'h00000000"
    val kseg0 = U"32'h00000000"
    val kseg1 = U"32'h00000000"
    val ksseg = U"32'hC0000000"
    val kseg3 = U"32'hE0000000"
  }

  for (i <- 0 until ports) {
    val unmapped = pipeToMmu.virtAddr(i) >= virtual.kseg0 && pipeToMmu.virtAddr(i) < virtual.kseg1
    val unmappedUncached = pipeToMmu.virtAddr(i) >= virtual.kseg1 && pipeToMmu.virtAddr(i) < virtual.ksseg
    val mapped = ~unmapped && ~unmappedUncached

    mmuToCacheBus.phyAddr(i) := MuxOH(
      Vec(
        unmapped,
        unmappedUncached,
        mapped
      ),
      Vec(
        pipeToMmu.virtAddr(i) - virtual.kseg0 + physical.kseg0,
        pipeToMmu.virtAddr(i) - virtual.kseg1 + physical.kseg1,
        mmuToTlbBus(i).dout.PFN @@ pipeToMmu.virtAddr(i)(11 downto 0)
      )
    )
    mmuToCacheBus.cached(i) := MuxOH(
      Vec(
        unmapped,
        unmappedUncached,
        mapped
      ),
      Vec(
        cp0ToMmuBus.k0Cached,
        False,
        mmuToTlbBus(i).dout.C === U(3, 3 bits)
      )
    )

    mmuToTlbBus(i).din.base.VPN2 := pipeToMmu.virtAddr(i)(31 downto 13)
    mmuToTlbBus(i).din.base.ASID := cp0ToMmuBus.ASID
    mmuToTlbBus(i).din.bias := pipeToMmu.virtAddr(i)(12)

    tlbException(i).refill := pipeToMmu.valid && mapped && (!mmuToTlbBus(i).hit)
    tlbException(i).invalid := pipeToMmu.valid && mapped && (mmuToTlbBus(i).hit && !mmuToTlbBus(i).dout.V)
    (_type == "D") generate (
      tlbException(i).modified := pipeToMmu.valid && pipeToMmu.isWrite(i) &&
        mapped && (mmuToTlbBus(i).hit && mmuToTlbBus(i).dout.V && !mmuToTlbBus(i).dout.D)
      )
  }
}

object MmuVerilog extends App {
  Config.spinal.generateVerilog(new Mmu("D", TlbConfig(3, 8), 2))
}