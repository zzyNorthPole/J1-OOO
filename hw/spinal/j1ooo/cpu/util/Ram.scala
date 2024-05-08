package j1ooo.cpu.util

import j1ooo.cpu.blackbox.{RamParameter, RamPort, vec_ram_sim, xpm_ram, xpm_ram_sim}
import j1ooo.gen.Config
import spinal.core._
import spinal.lib.slave

case class RamBase(
              _type: String,
              parameter: RamParameter
              ) extends Component {
  val io = slave(RamPort(parameter))
  noIoPrefix()

  (_type == "xpm") generate {
    val ram = new xpm_ram(parameter)
    io >> ram.io.port
  }

  (_type == "sim") generate {
    val ram = new xpm_ram_sim(parameter)
    io >> ram.io
  }

  (_type == "vec") generate {
    val ram = new vec_ram_sim(parameter)
    io >> ram.io
  }
}

case class Ram(
              _type: String,
              parameter: RamParameter
              ) extends Component {
  val io = slave(RamPort(parameter))
  noIoPrefix()

  import io._

  val ram = RamBase(_type, parameter)
  ram.io << io

  val banks = wea.getWidth
  val bankWidth = if (parameter.use4Data == false) parameter.width else 8
  (parameter._type == "tdpram") generate {
    val useA, useB = RegInit(B(0, banks bits))
    val dinaReg, dinbReg = Reg(UInt(parameter.width bits))
    for (i <- 0 until banks) {
      useB(i) := enb && web(i) && (addra === addrb)
    }
    dinbReg := dinb
    for (i <- 0 until banks) {
      douta(i << log2Up(bankWidth), bankWidth bits) := (
        useB(i) ?
          dinbReg(i << log2Up(bankWidth), bankWidth bits).asBits |
          ram.io.douta(i << log2Up(bankWidth), bankWidth bits).asBits
        ).asUInt
    }
    for (i <- 0 until banks) {
      useA(i) := ena && wea(i) && (addra === addrb)
    }
    dinaReg := dina
    for (i <- 0 until banks) {
      doutb(i << log2Up(bankWidth), bankWidth bits) := (
        useA(i) ?
          dinaReg(i << log2Up(bankWidth), bankWidth bits).asBits |
          ram.io.doutb(i << log2Up(bankWidth), bankWidth bits).asBits
      ).asUInt
    }
  }

  (parameter._type == "sdpram") generate {
    val useA = RegInit(B(0, banks bits))
    val dinaReg = Reg(UInt(parameter.width bits))
    for (i <- 0 until banks) {
      useA(i) := ena && wea(i) && (addra === addrb)
    }
    dinaReg := dina
    for (i <- 0 until banks) {
      doutb(i << log2Up(bankWidth), bankWidth bits) := (
        useA(i) ?
          dinaReg(i << log2Up(bankWidth), bankWidth bits).asBits |
          ram.io.doutb(i << log2Up(bankWidth), bankWidth bits).asBits
        ).asUInt
    }
  }

  (parameter._type == "dpdistram") generate {
    douta := ram.io.douta
    doutb := ram.io.doutb
  }
}

object RamVerilog extends App {
  Config.spinal.generateVerilog(Ram("xpm", RamParameter(256, 32, true, "tdpram")))
}