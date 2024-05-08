package j1ooo.cpu.fetch.bpu

import j1ooo.gen.{Config, PhtConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, slave}
case class PhtBus(phtConfig: PhtConfig, _type: String) extends Bundle with IMasterSlave {
  val history = UInt (phtConfig.indexWidth bits)
  val valid = (_type == "write") generate Bool()
  val count = UInt (2 bits)

  override def asMaster(): Unit = {
    out(history)
    (_type == "write") generate out(valid)
    (_type == "read") generate in(count)
    (_type == "write") generate out(count)
  }
}

class Pht(phtConfig: PhtConfig) extends Component {
  val io = new Bundle {
    val rPorts = Vec(slave(PhtBus(phtConfig, "read")), phtConfig.fetchPorts)
    val wPort = slave(PhtBus(phtConfig, "write"))
  }
  noIoPrefix()

  import io._

  val mem = Vec(Reg(UInt(phtConfig.lines bits)), phtConfig.fetchPorts)
  mem(0) init U(phtConfig.lines bits, default -> True)
  mem(1) init 0
  for (i <- 0 until phtConfig.fetchPorts) {
    for (j <- 0 until 2) {
      rPorts(i).count(j) := mem(j)(rPorts(i).history)
    }
  }

  when(wPort.valid) {
    for (i <- 0 until 2) {
      mem(i)(wPort.history) := wPort.count(i)
    }
  }
}

object PhtVerilog extends App {
  Config.spinal.generateVerilog(new Pht(PhtConfig(1 << 12, 2)))
}