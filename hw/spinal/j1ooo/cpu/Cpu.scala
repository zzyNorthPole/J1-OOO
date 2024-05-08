package j1ooo.cpu

import j1ooo.cpu.commit.{CommitPipeline, DebugSignal}
import j1ooo.cpu.decode.DecodePipeline
import j1ooo.cpu.execute.memory.{MemoryPipeline, MemoryRS}
import j1ooo.cpu.execute.muldiv.{MuldivPipeline, MuldivRS}
import j1ooo.cpu.execute.normal.{BypassInputBus, NormalPipeline, NormalRS}
import j1ooo.cpu.execute.{ARF, ROB, Rat}
import j1ooo.cpu.fetch.{FetchFIFO, FetchPipeline}
import j1ooo.gen.Config
import j1ooo.gen.Config.{axi4Config, bpuConfig, dCacheConfig, fetchFIFOConfig, iCacheConfig, lstConfig, mshrConfig, robConfig, rsConfig, storeBufferConfig, tlbConfig, writeBufferConfig, writeQueueConfig}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4R, Axi4ReadOnly, Axi4WriteOnly}
import spinal.lib.{master, memPimped}

class Cpu(useDebug: Boolean, _type: String) extends Component {
  val io = new Bundle {
    val extInt = in Bits(5 bits)

    val ibus = master(Axi4ReadOnly(axi4Config))

    val dbus = master(Axi4(axi4Config))

    val udbus = master(Axi4WriteOnly(axi4Config))

    val debug = (useDebug == true) generate master(DebugSignal())
  }
  noIoPrefix()
  val fetch = new FetchPipeline(iCacheConfig, bpuConfig, tlbConfig, fetchPortsNum = 2, _type)
  val fetchFIFO = new FetchFIFO(bpuConfig, fetchPorts = 2, fetchFIFOConfig, _type)
  val decode = new DecodePipeline(bpuConfig, robConfig, rsConfig, decodePorts = 2)
  val rat = new Rat(robConfig, decodePorts = 2)
  val rob = new ROB(robConfig, bpuConfig, 2)
  val arf = new ARF(2)
  val normalRs0 = new NormalRS(robConfig, rsConfig)
  val normalRs1 = new NormalRS(robConfig, rsConfig)
  val normal0 = new NormalPipeline(robConfig)
  val normal1 = new NormalPipeline(robConfig)
  val muldivRs = new MuldivRS(robConfig, rsConfig)
  val muldiv = new MuldivPipeline(robConfig)
  val memoryRs = new MemoryRS(robConfig, rsConfig)
  val memory = new MemoryPipeline(tlbConfig, dCacheConfig, mshrConfig, lstConfig, storeBufferConfig, writeBufferConfig, writeQueueConfig, robConfig, _type, commitPorts = 2)
  val commit = new CommitPipeline(true, tlbConfig, iCacheConfig, dCacheConfig, robConfig, bpuConfig, decodePorts = 2)

  fetch.io.flush := commit.io.flush
  fetch.io.flushPc := commit.io.targetPc
  fetch.io.flushRas := commit.io.curRas
  fetch.io.commitToBpuBus << commit.io.bpu
  fetch.io.commitToICacheOpBus << commit.io.icache
  fetch.io.cp0ToMmuBus << commit.io.mmu
  commit.io.tlbQuery(0) << fetch.io.mmuToTlbBus(0)
  commit.io.tlbQuery(1) << fetch.io.mmuToTlbBus(1)
  io.ibus << fetch.io.iBus

  fetchFIFO.io.flush := commit.io.flush
  fetchFIFO.io.input << fetch.io.output

  decode.io.flush := commit.io.flush
  decode.io.inputPorts << fetchFIFO.io.output
  for (i <- 0 until 4) {
    rat.io.rPorts(i).ratAddr := decode.io.ratRPorts(i).ratAddr
    decode.io.ratRPorts(i).ratItem := rat.io.rPorts(i).ratItem
  }
  for (i <- 0 until 2) {
    rat.io.wPorts(i) := decode.io.ratWPorts(i)
  }
  rob.io.flush := commit.io.flush
  rob.io.wPorts << decode.io.robPushPorts
  rob.io.queryPorts << decode.io.robQueryPorts
  arf.io.queryPorts << decode.io.arfQueryPorts

  normalRs0.io.flush := commit.io.flush
  normalRs0.io.inputPorts << decode.io.normalRsInputBus(0)

  normal0.io.flush := commit.io.flush
  normal0.io.inputPorts << normalRs0.io.outputPorts
  rob.io.normalWriteBackBus(0) << normal0.io.outputPorts

  normalRs1.io.flush := commit.io.flush
  normalRs1.io.inputPorts << decode.io.normalRsInputBus(1)

  normal1.io.flush := commit.io.flush
  normal1.io.inputPorts << normalRs1.io.outputPorts
  rob.io.normalWriteBackBus(1) << normal1.io.outputPorts

  muldivRs.io.flush := commit.io.flush
  muldivRs.io.inputPorts << decode.io.muldivRsInputBus

  muldiv.io.flush := commit.io.flush
  muldiv.io.inputPorts << muldivRs.io.outputPorts
  rob.io.muldivWriteBackBus << muldiv.io.outputPorts
  commit.io.mfc0 << muldiv.io.pipeToCp0BusMfc0
  for (i <- 0 until 2) {
    muldiv.io.commitPorts(i) << commit.io.hilo(i)
  }

  memoryRs.io.flush := commit.io.flush
  memoryRs.io.inputPorts << decode.io.memoryRsInputBus

  memory.io.flush := commit.io.flush
  memory.io.inputPorts << memoryRs.io.outputPorts
  memory.io.cp0ToMmuBus << commit.io.mmu
  commit.io.tlbQuery(2) << memory.io.mmuToTlbBus
  rob.io.memoryWriteBackBus << memory.io.outputPorts
  memory.io.commitToMshr << commit.io.mshr
  commit.io.mshrToRob << rob.io.mshrWriteBackBus
  memory.io.commitToCachedStore.valid := commit.io.cachedStore.valid
  commit.io.cachedStore.ready := memory.io.commitToCachedStore.ready
  io.dbus << memory.io.dbus
  io.udbus << memory.io.udbus

  val bypassPorts = Vec(BypassInputBus(robConfig), 9)
  for (i <- 0 until 2) bypassPorts(i) << normal0.io.bypassPorts(i)
  for (i <- 0 until 2) bypassPorts(i + 2) << normal1.io.bypassPorts(i)
  for (i <- 0 until 2) bypassPorts(i + 4) << muldiv.io.bypassPorts(i)
  for (i <- 0 until 3) bypassPorts(i + 6) << memory.io.bypassPorts(i)
  for (i <- 0 until 9) {
    normalRs0.io.bypassPorts(i) << bypassPorts(i)
    normalRs1.io.bypassPorts(i) << bypassPorts(i)
    muldivRs.io.bypassPorts(i) << bypassPorts(i)
    memoryRs.io.bypassPorts(i) << bypassPorts(i)
  }

  commit.io.extInt := io.extInt(4 downto 0)
  commit.io.commitPorts << rob.io.commitPorts

  arf.io.commitPorts << commit.io.arf

  rat.io.flush := commit.io.flush
  for (i <- 0 until 2) {
    rat.io.commitPorts(i).valid := commit.io.rat(i).valid
    rat.io.commitPorts(i).ratAddr := commit.io.rat(i).ratAddr
    rat.io.commitPorts(i).ratItem := commit.io.rat(i).ratItem
  }

  (useDebug == true) generate {
    commit.io.debug << io.debug
  }
}

object CpuVerilog extends App {
  Config.spinal.generateVerilog(new Cpu(true, "sim"))
}