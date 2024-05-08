package j1ooo.cpu.fetch.bpu

import j1ooo.cpu.blackbox.RamParameter
import j1ooo.cpu.util.Ram
import j1ooo.gen.BhtConfig
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class BhtBus(histories: Int, _type: String) extends Bundle with IMasterSlave {
  val pc = UInt(32 bits)
  val valid = (_type == "write") generate Bool()
  val history = UInt(histories bits)

  override def asMaster(): Unit = {
    out(pc)
    (_type == "write") generate out(valid)
    (_type == "read") generate in(history)
    (_type == "write") generate out(history)
  }
}

class Bht(bhtConfig: BhtConfig, _type: String) extends Component {
  val io = new Bundle {
    val rPorts = Vec(slave(BhtBus(bhtConfig.histories, "read")), bhtConfig.ways)
    val wPort = slave(BhtBus(bhtConfig.histories, "write"))
  }
  noIoPrefix()

  import io._

  val historyRams = Seq.fill(bhtConfig.ways) {
    Ram("vec", RamParameter(
      bhtConfig.lines, bhtConfig.histories, false, "dpdistram"
    ))
  }

  val readPortsInit = new Area {
    val pcUpd = Vec(UInt(32 bits), bhtConfig.ways)
    pcUpd(1) := rPorts(1).pc(2) ? rPorts(1).pc | rPorts(0).pc
    pcUpd(0) := rPorts(1).pc(2) ? rPorts(0).pc | rPorts(1).pc

    val indexes = Vec(UInt(bhtConfig.indexWidth bits), bhtConfig.ways)
    for (i <- 0 until 2) {
      indexes(i) := pcUpd(i)(3, bhtConfig.indexWidth bits)
    }

    val histories = Vec(UInt(bhtConfig.histories bits), bhtConfig.ways)
    for (i <- 0 until bhtConfig.ways) {
      historyRams(i).io.enb := True
      historyRams(i).io.addrb := indexes(i)
      histories(i) := historyRams(i).io.doutb
    }

    rPorts(1).history := rPorts(1).pc(2) ? histories(1) | histories(0)
    rPorts(0).history := rPorts(1).pc(2) ? histories(0) | histories(1)
  }

  val writePortsInit = new Area {
    val index = UInt(bhtConfig.indexWidth bits)
    index := wPort.pc(3, bhtConfig.indexWidth bits)
    for (i <- 0 until bhtConfig.ways) {
      historyRams(i).io.ena := wPort.valid && (U(i, 1 bits) === wPort.pc(2, 1 bits))
      historyRams(i).io.wea := B(1, 1 bits)
      historyRams(i).io.addra := index
      historyRams(i).io.dina := wPort.history
    }
  }
}
