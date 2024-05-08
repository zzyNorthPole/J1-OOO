package j1ooo.cpu.blackbox

import j1ooo.gen.Config
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class RamParameter(
                       depth: Int,
                       width: Int,
                       use4Data: Boolean,
                       _type: String
                       )
case class RamPort(parameter: RamParameter) extends Bundle with IMasterSlave {
  import parameter._
  // a for write
  val ena = Bool()
  val wea = Bits ((if (use4Data == false) 1 else width / 8) bits)
  val addra = UInt (log2Up(depth) bits)
  val dina = UInt (width bits)
  val douta = (_type != "sdpram") generate UInt (width bits)

  // b for read
  val enb = Bool()
  val web = (_type == "tdpram") generate Bits ((if (use4Data == false) 1 else width / 8) bits)
  val addrb = UInt (log2Up(depth) bits)
  val dinb = (_type == "tdpram") generate UInt (width bits)
  val doutb = UInt (width bits)

  override def asMaster(): Unit = {
    out(ena, wea, addra, dina)
    in(douta)
    out(enb, web, addrb, dinb)
    in(doutb)
  }

  def >>(that: RamPort) = {
    that.ena := this.ena
    that.wea := this.wea
    that.addra := this.addra
    that.dina := this.dina
    (_type != "sdpram") generate (this.douta := that.douta)

    that.enb := this.enb
    (_type == "tdpram") generate (that.web := this.web)
    that.addrb := this.addrb
    (_type == "tdpram") generate (that.dinb := this.dinb)
    this.doutb := that.doutb
  }

  def <<(that: RamPort) = {
    this.ena := that.ena
    this.wea := that.wea
    this.addra := that.addra
    this.dina := that.dina
    // (_type != "sdpram") generate (that.douta := this.douta)

    this.enb := that.enb
    (_type == "tdpram") generate (this.web := that.web)
    this.addrb := that.addrb
    (_type == "tdpram") generate (this.dinb := that.dinb)
    // that.doutb := this.doutb
  }
}

class xpm_ram(parameter: RamParameter) extends BlackBox {
  import parameter._
  addGeneric("ADDR_WIDTH_A", log2Up(depth))
  addGeneric("ADDR_WIDTH_B", log2Up(depth))
  addGeneric("BYTE_WRITE_WIDTH_A", if (use4Data == false) width else 8)
  (_type == "tdpram") generate addGeneric("BYTE_WRITE_WIDTH_B", if (use4Data == false) width else 8)
  addGeneric("CLOCKING_MODE", "common_clock")
  (_type != "dpdistram") generate addGeneric("MEMORY_PRIMITIVE", "block")
  addGeneric("MEMORY_SIZE", depth * width)
  (_type != "sdpram") generate addGeneric("READ_DATA_WIDTH_A", width)
  addGeneric("READ_DATA_WIDTH_B", width)
  (_type == "tdpram") generate addGeneric("READ_LATENCY_A", 1)
  (_type == "dpdistram") generate addGeneric("READ_LATENCY_A", 1)
  (_type != "dpdistram") generate addGeneric("READ_LATENCY_B", 1)
  (_type == "dpdistram") generate addGeneric("READ_LATENCY_B", 0)
  addGeneric("WRITE_DATA_WIDTH_A", width)
  (_type == "tdpram") generate addGeneric("WRITE_DATA_WIDTH_B", width)
  (_type == "tdpram") generate addGeneric("WRITE_MODE_A", "read_first")
  (_type != "dpdistram") generate addGeneric("WRITE_MODE_B", "read_first")

  val io = new Bundle {
    val clka = in Bool()
    val port = slave(RamPort(parameter))
  }

  noIoPrefix()

  mapClockDomain(clock = io.clka)
}

class xpm_ram_sim(parameter: RamParameter) extends Component {
  val io = slave(RamPort(parameter))

  noIoPrefix()

  import parameter._
  import io._

  val mem = Mem(UInt(width bits), depth)

  (_type == "tdpram") generate (
    douta := mem.readWriteSync(
      address = addra,
      data = dina,
      enable = ena,
      write = wea.orR,
      mask = wea
    )
  )
  (_type == "sdpram") generate (
    mem.write(
      address = addra,
      data = dina,
      enable = ena && wea.orR,
      mask = wea
    )
  )
  (_type == "dpdistram") generate {
    mem.write(
      address = addra,
      data = dina,
      enable = ena && wea.orR,
      mask = wea
    )
    douta := mem.readAsync(address = addra)
  }

  (_type == "tdpram") generate (
    doutb := mem.readWriteSync(
      address = addrb,
      data = dinb,
      enable = enb,
      write = web.orR,
      mask = web
    )
  )
  (_type == "sdpram") generate (
    doutb := mem.readSync(
      address = addrb,
      enable = enb
    )
  )
  (_type == "dpdistram") generate (
    doutb := mem.readAsync(address = addrb)
  )
}

class vec_ram_sim(parameter: RamParameter) extends Component {
  val io = slave(RamPort(parameter))

  noIoPrefix()

  import parameter._
  import io._

  val banks = wea.getWidth
  assert(banks > 0)
  val bankWidth = if (use4Data == false) width else 8
  val mem = Vec(RegInit(U(0, (depth * bankWidth) bits)), banks)

  (_type == "tdpram") generate {
    val splitDina = Vec(UInt(bankWidth bits), banks)
    for (i <- 0 until banks) {
      splitDina(i) := dina(i << log2Up(bankWidth), bankWidth bits)
    }
    for (i <- 0 until banks) {
      when(wea(i) && ena) {
        mem(i)(addra << log2Up(bankWidth), bankWidth bits) := splitDina(i)
      }
    }
    val splitDouta = Vec(RegInit(U(0, bankWidth bits)), banks)
    for (i <- 0 until banks) {
      when(ena) {
        splitDouta(i) := mem(i)(addra << log2Up(bankWidth), bankWidth bits)
      }
    }
    for (i <- 0 until banks) {
      douta(i << log2Up(bankWidth), bankWidth bits) := splitDouta(i)
    }
  }
  (_type == "sdpram") generate {
    val splitDina = Vec(UInt(bankWidth bits), banks)
    for (i <- 0 until banks) {
      splitDina(i) := dina(i << log2Up(bankWidth), bankWidth bits)
    }
    for (i <- 0 until banks) {
      when(wea(i) && ena) {
        mem(i)(addra << log2Up(bankWidth), bankWidth bits) := splitDina(i)
      }
    }
  }
  (_type == "dpdistram") generate {
    val splitDina = Vec(UInt(bankWidth bits), banks)
    for (i <- 0 until banks) {
      splitDina(i) := dina(i << log2Up(bankWidth), bankWidth bits)
    }
    for (i <- 0 until banks) {
      when(wea(i) && ena) {
        mem(i)(addra << log2Up(bankWidth), bankWidth bits) := splitDina(i)
      }
    }
    for (i <- 0 until banks) {
      douta(i << log2Up(bankWidth), bankWidth bits) := mem(i)(addra << log2Up(bankWidth), bankWidth bits)
    }
  }

  (_type == "tdpram") generate {
    val splitDinb = Vec(UInt(bankWidth bits), banks)
    for (i <- 0 until banks) {
      splitDinb(i) := dinb(i << log2Up(bankWidth), bankWidth bits)
    }
    for (i <- 0 until banks) {
      when(web(i) && enb) {
        mem(i)(addrb << log2Up(bankWidth), bankWidth bits) := splitDinb(i)
      }
    }
    val splitDoutb = Vec(RegInit(U(0, bankWidth bits)), banks)
    for (i <- 0 until banks) {
      when(enb) {
        splitDoutb(i) := mem(i)(addrb << log2Up(bankWidth), bankWidth bits)
      }
    }
    for (i <- 0 until banks) {
      doutb(i << log2Up(bankWidth), bankWidth bits) := splitDoutb(i)
    }
  }
  (_type == "sdpram") generate {
    val splitDoutb = Vec(RegInit(U(0, bankWidth bits)), banks)
    for (i <- 0 until banks) {
      when(enb) {
        splitDoutb(i) := mem(i)(addrb << log2Up(bankWidth), bankWidth bits)
      }
    }
    for (i <- 0 until banks) {
      doutb(i << log2Up(bankWidth), bankWidth bits) := splitDoutb(i)
    }
  }
  (_type == "dpdistram") generate {
    for (i <- 0 until banks) {
      doutb(i << log2Up(bankWidth), bankWidth bits) := mem(i)(addrb << log2Up(bankWidth), bankWidth bits)
    }
  }
}
