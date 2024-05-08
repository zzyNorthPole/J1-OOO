package j1ooo.gen

import j1ooo.axi.AxiCrossbar
import j1ooo.cpu.Cpu
import j1ooo.cpu.commit.DebugSignal
import j1ooo.gen.Config.axi4Config
import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.master

class mycpu_top(useDebug: Boolean, _type: String) extends Component {
  setDefinitionName("mycpu_top")
  val io = new Bundle {
    val aclk = in Bool()
    val aresetn = in Bool()

    val ext_int = in Bits(6 bits)
    val wid = out UInt(4 bits)
    val cpuBus = master(Axi4(axi4Config))
    val arlock = out Bits(2 bits)
    val awlock = out Bits(2 bits)

    val debug = (useDebug == true) generate out(DebugSignal())
  }
  noIoPrefix()
  import io._

  cpuBus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(1) + tmpList(tmpList.size - 1))
    }
  }

  val clk = Bool()
  clk := io.aclk
  val reset = Bool()
  reset := ~io.aresetn
  wid := U(1, 4 bits)
  val defaultClockDomain = ClockDomain(
    clock = clk,
    reset = reset,
    config = Config.spinal.defaultConfigForClockDomains
  )
  val defaultClockArea = new ClockingArea(defaultClockDomain) {
    val cpu = new Cpu(useDebug, _type)
    val axiCrossbar = new AxiCrossbar(axi4Config)

    cpu.io.ibus >> axiCrossbar.io.ibus
    cpu.io.dbus >> axiCrossbar.io.dbus
    cpu.io.udbus >> axiCrossbar.io.udbus
    arlock := B(0, 2 bits)
    awlock := B(0, 2 bits)

    axiCrossbar.io.cpuBus >> cpuBus

    cpu.io.extInt := ext_int(4 downto 0)

    (useDebug == true) generate {
      debug.aclk.clear()
      cpu.io.debug.aclk := aclk
      debug.wb := cpu.io.debug.wb
      debug.cp0 := cpu.io.debug.cp0
      debug.int := cpu.io.debug.int
      debug.commit := cpu.io.debug.commit
    }
  }
}

object TestVerilog extends App {
  Config.spinal.generateVerilog(new mycpu_top(true, "sim"))
}