package j1ooo.cpu.commit

import j1ooo.cpu.mmu.{Cp0ToMmuBus, TLBPBus, TLBPhy, TLBReadBus, TLBVirt, TLBWriteBus}
import j1ooo.cpu.signal.{Cp0Reg, Exception}
import j1ooo.cpu.util.Lfsr
import j1ooo.gen.{CacheConfig, Config, TlbConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, MuxOH, master, slave}

case class PipeToCp0BusTlbOp() extends Bundle with IMasterSlave {
  val valid = Bool()
  val tlbp = new Bundle {
    val valid = Bool()
  }
  val tlbr = new Bundle {
    val valid = Bool()
  }
  val tlbw = new Bundle {
    val valid = Bool()
    val isWr = Bool() // 1 for tlbwr 0 for tlbwi
  }
  val pc = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, tlbp, tlbr, tlbw, pc)
  }
}

case class PipeToCp0BusMtc0() extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = Cp0Reg()
  val select = UInt(3 bits)
  val din = UInt(32 bits)
  val pc = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, addr, select, din, pc)
  }
}

case class PipeToCp0BusMfc0() extends Bundle with IMasterSlave {
  val addr = Cp0Reg()
  val select = UInt(3 bits)
  val dout = UInt(32 bits)

  override def asMaster(): Unit = {
    out(addr, select)
    in(dout)
  }

  def << (that: PipeToCp0BusMfc0): Unit = {
    this.addr := that.addr
    this.select := that.select
    that.dout := this.dout
  }
}

// movz/movn don't change cp0
// transfer into cp0 to simplify the procedure of raise target pc
// movz/movn's behavior is similar to tlb operation and mtc0
case class PipeToCp0BusMov() extends Bundle with IMasterSlave {
  val valid = Bool()
  val pc = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, pc)
  }
}

case class PipeToCp0BusException() extends Bundle with IMasterSlave {
  val valid = Bool()
  val op = Exception()
  val isTlbHit = Bool()
  val bad = new Bundle {
    val pc = UInt(32 bits)
    val delaySlot = Bool()
    val addr = UInt(32 bits)
    val vpn2 = UInt(19 bits)
  }

  override def asMaster(): Unit = {
    out(valid, op, isTlbHit, bad)
  }
}

case class Cp0ToPcRegBus() extends Bundle with IMasterSlave {
  val flush = Bool()
  val pc = UInt(32 bits)

  override def asMaster(): Unit = {
    out(flush, pc)
  }
}

class Cp0(useDebug: Boolean, tlbConfig: TlbConfig, iCacheConfig: CacheConfig, dCacheConfig: CacheConfig) extends Component {
  val io = new Bundle {
    val pipe = new Bundle {
      // tlb operation control signal from pipeline
      val tlbOp = slave(PipeToCp0BusTlbOp())

      // mtc0 operation control signal from pipeline
      val mtc0 = slave(PipeToCp0BusMtc0())

      // mfc0 operation control signal from pipeline
      val mfc0 = slave(PipeToCp0BusMfc0())

      val mov = slave(PipeToCp0BusMov())

      // eret
      val eret = in Bool()

      // exception
      val exception = slave(PipeToCp0BusException())
    }

    // interrupt
    val extInt = in Bits(5 bits)
    val interrupt = out Bool()

    val mmu = master(Cp0ToMmuBus())

    val tlb = new Bundle {
      val wPorts = master(TLBWriteBus(tlbConfig))
      val rPorts = master(TLBReadBus(tlbConfig))
      val pPorts = master(TLBPBus(tlbConfig))
    }

    // flush signal caused by mtc0/tlb operation/exception/interrupt/eret
    val pcReg = master(Cp0ToPcRegBus())

    val debug = (useDebug == true) generate new Bundle {
      val count = out UInt(32 bits)
      val random = out UInt(32 bits)
      val cause = out UInt(32 bits)
    }
  }
  noIoPrefix()

  import io._

  // cp0 reg 0, select 0
  val Index = new Bundle {
    val P = RegInit(U(0, 1 bits))
    val Index = RegInit(U(0, log2Up(tlbConfig.lines) bits))
  }
  val index = UInt(32 bits)
  index := Index.P @@ U(0, 32 - log2Up(tlbConfig.lines) - 1 bits) @@ Index.Index

  // cp0 reg 1, select 0
  val Random = new Bundle {
    val Random = RegInit(U(log2Up(tlbConfig.lines) bits, default -> True))
  }
  val random = UInt(32 bits)
  random := U(0, 32 - log2Up(tlbConfig.lines) bits) @@ Random.Random

  // cp0 reg 2, select 0
  // cp0 reg 3, select 0
  val EntryLo = Vec(
    new Bundle {
      val Fill = U(0, 6 bits)
      val Phy = Reg(TLBPhy()).init()
      val G = RegInit(False)
    },
    2
  )
  val entryLo = Vec(UInt(32 bits), 2)
  for (i <- 0 until 2) {
    entryLo(i) := EntryLo(i).Fill @@ EntryLo(i).Phy.combine() @@ EntryLo(i).G.asUInt
  }

  // cp0 reg 4, select 0
  val Context = new Bundle {
    val PTEBase = RegInit(U(0, 9 bits))
    val BadVPN2 = RegInit(U(0, 19 bits))
  }
  val context = UInt(32 bits)
  context := Context.PTEBase @@ Context.BadVPN2 @@ U(0, 4 bits)

  // cp0 reg 5, select 0
  val PageMask = new Bundle {
    val Mask = U(0, 16 bits)
  }
  val pageMask = UInt(32 bits)
  pageMask := U(0, 3 bits) @@ PageMask.Mask @@ U(0, 13 bits)

  // cp0 reg 6, select 0
  val Wired = new Bundle {
    val Wired = RegInit(U(0, log2Up(tlbConfig.lines) bits))
  }
  val wired = UInt(32 bits)
  wired := U(0, 32 - log2Up(tlbConfig.lines) bits) @@ Wired.Wired

  // cp0 reg 8, select 0
  val BadVAddr = new Bundle {
    val BadVAddr = RegInit(U(0, 32 bits))
  }
  val badVAddr = UInt(32 bits)
  badVAddr := BadVAddr.BadVAddr

  // cp0 reg 9, select 0
  val Count = new Bundle {
    val Count = RegInit(U(0, 32 bits))
  }
  val count = UInt(32 bits)
  count := Count.Count

  val EntryHi = Reg(TLBVirt()).init()
  val entryHi = UInt(32 bits)
  entryHi := EntryHi.VPN2 @@ U(0, 5 bits) @@ EntryHi.ASID

  // cp0 reg 11, select 0
  val Compare = new Bundle {
    val Compare = RegInit(U(0, 32 bits))
  }
  val compare = UInt(32 bits)
  compare := Compare.Compare

  // cp0 reg 12, select 0
  val Status = new Bundle {
    val CU = RegInit(U(0, 1 bits))
    val BEV = RegInit(U(1, 1 bits))
    val IM = RegInit(U(0, 8 bits))
    val UM = RegInit(U(0, 1 bits))
    val ERL = RegInit(U(0, 1 bits)) // optional
    val EXL = RegInit(U(0, 1 bits))
    val IE = RegInit(U(0, 1 bits))
  }
  val status = UInt(32 bits)
  status :=
    U(0, 3 bits) @@ Status.CU @@ U(0, 5 bits) @@ Status.BEV @@ U(0, 6 bits) @@ Status.IM @@
      U(0, 3 bits) @@ Status.UM @@ U(0, 1 bits) @@ Status.ERL @@ Status.EXL @@ Status.IE

  // cp0 reg 13, select 0
  val Cause = new Bundle {
    val BD = RegInit(U(0, 1 bits))
    val IV = RegInit(U(0, 1 bits)) // optional
    val IP = RegInit(U(0, 8 bits))
    val Exc = RegInit(U(0, 5 bits))
  }
  val cause = UInt(32 bits)
  cause := Cause.BD @@ U(0, 7 bits) @@ Cause.IV @@ U(0, 7 bits) @@ Cause.IP @@
    U(0, 1 bits) @@ Cause.Exc @@ U(0, 2 bits)

  // cp0 reg 14, select 0
  val EPC = new Bundle {
    val EPC = RegInit(U(0, 32 bits))
  }
  val epc = UInt(32 bits)
  epc := EPC.EPC

  // cp0 reg 15, select 0
  val PRId = new Bundle {
    val PRId = U"32'h00018003"
  }
  val prid = UInt(32 bits)
  prid := PRId.PRId

  // cp0 reg 15, select 1
  val EBase = new Bundle {
    val EBase = RegInit(U"32'h80000000")
  }
  val eBase = UInt(32 bits)
  eBase := EBase.EBase

  // cp0 reg 16, select 0
  val Config0 = new Bundle {
    val M = U(1, 1 bits) // 1 denotes config1 register is implemented
    val BE = U(0, 1 bits) // 0 for little endian
    val AT = U(0, 2 bits) // 0 for MIPS32
    val AR = U(0, 3 bits) // 0 for MIPS32 release 1
    val MT = U(1, 3 bits) // 1 for standard TLB
    val VI = U(0, 1 bits) // 0 denotes instruction cache is not virtual(using both virtual indexing and virtual tags)
    val K0 = RegInit(U(3, 3 bits)) // kseg0 cacheability and coherency attribute
  }
  val config0 = UInt(32 bits)
  config0 := Config0.M @@ U(0, 15 bits) @@ Config0.BE @@ Config0.AT @@
    Config0.AR @@ Config0.MT @@ U(0, 3 bits) @@ Config0.VI @@ Config0.K0

  // cp0 reg 16, select 1
  val Config1 = new Bundle {
    val M = U(0, 1 bits) // 0 denotes config 2 register is not implemented
    val MMUSize = U(tlbConfig.lines - 1, 6 bits) // number of entries in the tlb
    val IS = U(log2Up(iCacheConfig.lines) - 6, 3 bits) // icache sets per way: 0-64
    val IL = U(log2Up(iCacheConfig.blockSize) - 1, 3 bits) // icache line size: 1-4 bytes
    val IA = U(iCacheConfig.ways - 1, 3 bits) // icache associativity: 1-2 way
    val DS = U(log2Up(dCacheConfig.lines) - 6, 3 bits) // dcache sets per way: 0-64
    val DL = U(log2Up(dCacheConfig.blockSize) - 1, 3 bits) // dcache line size: 1-4 bytes
    val DA = U(dCacheConfig.ways - 1, 3 bits) // dcache associativity: 1-2 way
    val C2 = U(0, 1 bits) // no coprocessor 2 implemented
    val MD = U(0, 1 bits)
    val PC = U(0, 1 bits) // no performance counter registers implemented
    val WR = U(0, 1 bits) // no watch registers implemented
    val CA = U(0, 1 bits) // mips163 not implemented
    val EP = U(0, 1 bits) // no EJTAG implemented
    val FP = U(0, 1 bits) // no FPU implemented
  }
  val config1 = UInt(32 bits)
  config1 := Config1.M @@ Config1.MMUSize @@
    Config1.IS @@ Config1.IL @@ Config1.IA @@
    Config1.DS @@ Config1.DL @@ Config1.DA @@
    Config1.C2 @@ Config1.MD @@ Config1.PC @@ Config1.WR @@ Config1.CA @@ Config1.EP @@ Config1.FP

  // cp0 reg 30, select 1 (optional)
  val ErrorEPC = new Bundle {
    val ErrorEPC = RegInit(U(0, 32 bits))
  }
  val errorEPC = UInt(32 bits)
  errorEPC := ErrorEPC.ErrorEPC

  // cp0 reg 0, select 0
  when(pipe.tlbOp.tlbp.valid) {
    Index.P := (!tlb.pPorts.hit).asUInt
    when(tlb.pPorts.hit) {
      Index.Index := tlb.pPorts.addr
    }
  }.elsewhen(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Index) {
    Index.Index := pipe.mtc0.din(log2Up(tlbConfig.lines) - 1 downto 0)
  }
  tlb.rPorts.addr := index(log2Up(tlbConfig.lines) - 1 downto 0)

  // cp0 reg 1, select 0
  val lfsrWidth = log2Up(tlbConfig.lines) + 2
  val lfsr = new Lfsr(lfsrWidth)
  lfsr.io.en := True
  lfsr.io.seed := U((lfsrWidth - 1 downto 1) -> False, 0 -> True)
  val lfsrDout = UInt(lfsrWidth bits)
  lfsrDout := lfsr.io.dout
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Wired) {
    Random.Random := pipe.mtc0.din(log2Up(tlbConfig.lines) - 1 downto 0)
  }.otherwise {
    when(Random.Random === U(tlbConfig.lines - 1, log2Up(tlbConfig.lines) bits)) {
      Random.Random := Wired.Wired
    }.otherwise {
      Random.Random := Random.Random + (U(0, log2Up(tlbConfig.lines) - 1 bits) @@ lfsrDout(1))
    }
  }
  tlb.wPorts.addr := (pipe.tlbOp.tlbw.isWr ? random | index)(log2Up(tlbConfig.lines) - 1 downto 0)

  // cp0 reg 2, select 0
  // cp0 reg 3, select 0
  when(pipe.tlbOp.tlbr.valid) {
    for (i <- 0 until 2) {
      EntryLo(i).Phy := tlb.rPorts.dout.Phy(i)
      EntryLo(i).G := tlb.rPorts.dout.G
    }
  }.otherwise {
    for (i <- 0 until 2) {
      when(
        pipe.mtc0.valid &&
          pipe.mtc0.addr === (if (i == 0) Cp0Reg.EntryLo0 else Cp0Reg.EntryLo1)
      ) {
        EntryLo(i).Phy.PFN := pipe.mtc0.din(25 downto 6)
        EntryLo(i).Phy.C := pipe.mtc0.din(5 downto 3)
        EntryLo(i).Phy.D := pipe.mtc0.din(2)
        EntryLo(i).Phy.V := pipe.mtc0.din(1)
        EntryLo(i).G := pipe.mtc0.din(0)
      }
    }
  }
  for (i <- 0 until 2) {
    tlb.wPorts.din.Phy(i) := EntryLo(i).Phy
  }
  tlb.wPorts.din.G := EntryLo(0).G & EntryLo(1).G

  // cp0 reg 4, select 0
  when(pipe.exception.valid && (pipe.exception.op === Exception.TLBL || pipe.exception.op === Exception.TLBS || pipe.exception.op === Exception.MOD)) {
    Context.BadVPN2 := pipe.exception.bad.vpn2
  }.elsewhen(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Context) {
    Context.PTEBase := pipe.mtc0.din(31 downto 23)
  }

  // cp0 reg 6, select 0
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Wired) {
    Wired.Wired := pipe.mtc0.din(log2Up(tlbConfig.lines) - 1 downto 0)
  }

  // cp0 reg 8, select 0
  when(pipe.exception.valid && (
    pipe.exception.op === Exception.ADEL || pipe.exception.op === Exception.ADES ||
      pipe.exception.op === Exception.TLBL || pipe.exception.op === Exception.TLBS || pipe.exception.op === Exception.MOD
    )) {
    BadVAddr.BadVAddr := pipe.exception.bad.addr
  }

  // cp0 reg 9, select 0
  val tikiTaka = RegInit(False)
  tikiTaka := ~tikiTaka
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Count) {
    Count.Count := pipe.mtc0.din
  }.elsewhen(tikiTaka) {
    Count.Count := Count.Count + 1
  }

  // cp0 reg 10, select 0
  when(pipe.exception.valid && (pipe.exception.op === Exception.TLBL || pipe.exception.op === Exception.TLBS || pipe.exception.op === Exception.MOD)) {
    EntryHi.VPN2 := pipe.exception.bad.vpn2
  }.elsewhen(pipe.tlbOp.tlbr.valid) {
    EntryHi := tlb.rPorts.dout.Virt
  }.elsewhen(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.EntryHi) {
    EntryHi.VPN2 := pipe.mtc0.din(31 downto 13)
    EntryHi.ASID := pipe.mtc0.din(7 downto 0)
  }
  tlb.pPorts.din := EntryHi
  tlb.wPorts.din.Virt := EntryHi
  mmu.ASID := EntryHi.ASID

  // cp0 reg 11, select 0
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Compare) {
    Compare.Compare := pipe.mtc0.din
  }

  // cp0 reg 12, select 0
  when(pipe.eret) {
    when(Status.ERL === U(1, 1 bits)) {
      Status.ERL := 0
    }.otherwise {
      Status.EXL := 0
    }
  }.elsewhen(pipe.exception.valid) {
    Status.EXL := 1
  }.elsewhen(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Status) {
    Status.CU(0) := pipe.mtc0.din(28)
    Status.BEV := pipe.mtc0.din(22 downto 22)
    Status.IM := pipe.mtc0.din(15 downto 8)
    Status.UM := pipe.mtc0.din(4 downto 4)
    Status.ERL := pipe.mtc0.din(2 downto 2) // optional
    Status.EXL := pipe.mtc0.din(1 downto 1)
    Status.IE := pipe.mtc0.din(0 downto 0)
  }

  // cp0 reg 13, select 0
  when(pipe.exception.valid && Status.EXL === U"1'b0") {
    Cause.BD := pipe.exception.bad.delaySlot.asUInt
  }
  Cause.IP(7) := Cause.IP(7) | (count === compare)
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Compare) {
    Cause.IP(7) := False
  }
  Cause.IP(6 downto 2) := extInt.asUInt
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Cause) {
    Cause.IV := pipe.mtc0.din(23 downto 23)
    Cause.IP(1 downto 0) := pipe.mtc0.din(9 downto 8)
  }
  when(pipe.exception.valid) {
    Cause.Exc := U(0, 1 bits) @@ pipe.exception.op.asBits.asUInt
  }

  // cp0 reg 14, select 0
  when(pipe.exception.valid && Status.EXL === U(0, 1 bits)) {
    EPC.EPC := pipe.exception.bad.delaySlot ? (pipe.exception.bad.pc - 4) | pipe.exception.bad.pc
  }.elsewhen(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.EPC) {
    EPC.EPC := pipe.mtc0.din
  }

  // cp0 reg 15, select 1
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.PRIdEBase && pipe.mtc0.select === U(1, 3 bits)) {
    EBase.EBase := U"32'h80000000" | (pipe.mtc0.din & U"32'h3ffff000")
  }

  // cp0 reg 16, select 0
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.Config && pipe.mtc0.select === U(0, 3 bits)) {
    Config0.K0 := pipe.mtc0.din(2 downto 0)
  }
  mmu.k0Cached := Config0.K0 === U(3, 3 bits)

  // cp0 reg 30, select 0
  when(pipe.mtc0.valid && pipe.mtc0.addr === Cp0Reg.ErrorEPC && pipe.mtc0.select === U(0, 3 bits)) {
    ErrorEPC.ErrorEPC := pipe.mtc0.din
  }

  // signal across cp0
  tlb.wPorts.valid := pipe.tlbOp.tlbw.valid

  // to pc reg for flush
  // mtc0/tlb operation/exception/interrupt/eret
  pcReg.flush := pipe.eret || pipe.tlbOp.valid || pipe.exception.valid || pipe.mtc0.valid || pipe.mov.valid
  val tlbRefillException = (pipe.exception.op === Exception.TLBL || pipe.exception.op === Exception.TLBS) && !pipe.exception.isTlbHit && !Status.EXL(0)
  val intException = (pipe.exception.op === Exception.INT) && Cause.IV(0) && !Status.BEV(0) && !Status.EXL(0)
  val eretBase = (Status.ERL === U(1, 1 bits)) ? errorEPC | epc
  val exceptionBase = (Status.BEV === U(1, 1 bits)) ? U"32'hBFC00200" | eBase
  pcReg.pc := MuxOH(
    Vec(
      pipe.eret,
      pipe.exception.valid && tlbRefillException,
      pipe.exception.valid && intException,
      pipe.exception.valid && !tlbRefillException && !intException,
      pipe.tlbOp.valid,
      pipe.mtc0.valid,
      pipe.mov.valid
    ),
    Vec(
      eretBase,
      exceptionBase,
      exceptionBase + U"32'h00000200",
      exceptionBase + U"32'h00000180",
      pipe.tlbOp.pc + 4,
      pipe.mtc0.pc + 4,
      pipe.mov.pc + 4
    )
  )

  // IM: 15 downto 8
  // ERL: 2 downto 2
  // EXL: 1 downto 1
  // IE: 0 downto 0
  interrupt := status(0) && !status(1) && !status(2) && (status(15 downto 8) & Cause.IP).orR

  pipe.mfc0.dout := pipe.mfc0.addr.mux(
    Cp0Reg.Index -> index,
    Cp0Reg.Random -> random,
    Cp0Reg.EntryLo0 -> entryLo(0),
    Cp0Reg.EntryLo1 -> entryLo(1),
    Cp0Reg.Context -> context,
    Cp0Reg.PageMask -> pageMask,
    Cp0Reg.Wired -> wired,
    Cp0Reg.BadVAddr -> badVAddr,
    Cp0Reg.Count -> count,
    Cp0Reg.EntryHi -> entryHi,
    Cp0Reg.Compare -> compare,
    Cp0Reg.Status -> status,
    Cp0Reg.Cause -> cause,
    Cp0Reg.EPC -> epc,
    Cp0Reg.PRIdEBase -> ((pipe.mfc0.select === U(0, 3 bits)) ? prid | eBase),
    Cp0Reg.Config -> ((pipe.mfc0.select(2 downto 1) === U(0, 2 bits)) ? ((pipe.mfc0.select(0) === True) ? config1 | config0) | U(0, 32 bits)),
    Cp0Reg.ErrorEPC -> errorEPC
  )

  (useDebug == true) generate {
    debug.count := count
    debug.random := random
    debug.cause := cause
  }

}

object Cp0Verilog extends App {
  Config.spinal.generateVerilog(new Cp0(true, TlbConfig(3, 8), CacheConfig(2, 1 << 7, 64), CacheConfig(2, 1 << 7, 64)))
}