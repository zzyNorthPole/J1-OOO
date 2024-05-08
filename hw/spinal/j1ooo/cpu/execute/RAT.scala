package j1ooo.cpu.execute

import j1ooo.gen.{Config, RobConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class RatItem(robConfig: RobConfig) extends Bundle {
  val robAddr = UInt(log2Up(robConfig.lines) bits)
  val inRob = Bool()
}

case class RatBus(robConfig: RobConfig, _type: String) extends Bundle with IMasterSlave {
  val valid = (_type == "write" || _type == "commit") generate Bool()
  val ratAddr = UInt(log2Up(32) bits)
  val ratItem = RatItem(robConfig)

  override def asMaster(): Unit = {
    (_type == "write" || _type == "commit") generate {
      out(valid, ratAddr, ratItem)
    }
    (_type == "read") generate {
      out(ratAddr)
      in(ratItem)
    }
  }
}

class Rat(robConfig: RobConfig, decodePorts: Int) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val rPorts = Vec(slave(RatBus(robConfig, "read")), decodePorts * 2)

    val wPorts = Vec(slave(RatBus(robConfig, "write")), decodePorts)

    val commitPorts = Vec(slave(RatBus(robConfig, "commit")), decodePorts)
  }
  noIoPrefix()

  import io._
  val rat = Vec(Reg(RatItem(robConfig)), 32)

  // target == 0 arf
  // target == 1 rob
  when(flush) {
    for (i <- 0 until 32) {
      rat(i).inRob.clear()
    }
  }

  for (i <- 0 until decodePorts * 2) {
    rPorts(i).ratItem := rat(rPorts(i).ratAddr)
  }

  for (i <- 0 until decodePorts) {
    when(!flush && commitPorts(i).valid && commitPorts(i).ratItem.robAddr === rat(commitPorts(i).ratAddr).robAddr) {
      rat(commitPorts(i).ratAddr).inRob := False
    }
  }

  for (i <- 0 until decodePorts) {
    when(!flush && wPorts(i).valid) {
      rat(wPorts(i).ratAddr) := wPorts(i).ratItem
    }
  }
}

object RatVerilog extends App {
  Config.spinal.generateVerilog(new Rat(Config.robConfig, 2))
}