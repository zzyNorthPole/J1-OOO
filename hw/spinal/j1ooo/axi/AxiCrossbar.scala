package j1ooo.axi

import j1ooo.gen.Config
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4ReadOnly, Axi4WriteOnly}
import spinal.lib._

class AxiCrossbar(axi4Config: Axi4Config) extends Component {
  val io = new Bundle {
    val ibus = slave(Axi4ReadOnly(axi4Config)).setBlocked()
    val dbus = slave(Axi4(axi4Config)).setBlocked()
    val udbus = slave(Axi4WriteOnly(axi4Config)).setBlocked()
    val cpuBus = master(Axi4(axi4Config)).setIdle()
  }
  noIoPrefix()

  // in this axiCrossbar, we separate ar/r and aw/w/b

  // when two bus's req arrived at the same time,
  // dbus/udbus > ibus as verilog's properties

  // when a bus is firing, (record it as 0 clock)
  // 1 clock will set busy to 0, and 2 clock will arbitrate new request

  // but we shouldn't concern about it,
  // as what we need to concentrate on the different behavior in operate on the three bus!

  // thanks for nscscc2022 Tsinghua University ZenCove's axiCrossbar

  import io._
  import io._

  cpuBus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(1) + tmpList(tmpList.size - 1))
    }
  }

  val arBus = List(ibus.ar, dbus.ar)
  val rBus = List(ibus.r, dbus.r)
  val arReq = arBus.map(_.valid).orR
  val rBusy = RegInit(False)
  val rArbitrate = RegInit(U"0")
  when(!rBusy && arReq) {
    rBusy := True
    for (i <- 0 to 1) {
      when(arBus(i).valid === True) {
        rArbitrate := i
      }
    }
  }
  when(rBusy) {
    for (i <- 0 to 1) {
      when(rArbitrate === i) {
        arBus(i) >> cpuBus.ar
        rBus(i) << cpuBus.r
      }
    }
  }
  when(cpuBus.r.fire && cpuBus.r.last) {
    rBusy := False
  }

  val awBus = List(dbus.aw, udbus.aw)
  val wBus = List(dbus.w, udbus.w)
  val bBus = List(dbus.b, udbus.b)
  val awReq = awBus.map(_.valid).orR
  val wBusy = RegInit(False)
  val wArbitrate = Reg(U"0")
  when(!wBusy && awReq) {
    wBusy := True
    for (i <- 0 to 1) {
      when(awBus(i).valid === True) {
        wArbitrate := i
      }
    }
  }
  when(wBusy) {
    for (i <- 0 to 1) {
      when(wArbitrate === i) {
        awBus(i) >> cpuBus.aw
        wBus(i) >> cpuBus.w
        bBus(i) << cpuBus.b
      }
    }
  }
  when(cpuBus.b.fire) {
    wBusy := False
  }
}

object AxiCrossbarVerilog extends App {
  Config.spinal.generateVerilog(new AxiCrossbar(Config.axi4Config))
}