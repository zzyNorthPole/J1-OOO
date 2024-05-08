package j1ooo.cpu.execute

import j1ooo.gen.Config
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class ArfQueryBus(decodePorts: Int) extends Bundle with IMasterSlave {
  val addr = Vec(Vec(UInt(log2Up(32) bits), 2), decodePorts)
  val data = Vec(Vec(UInt(32 bits), 2), decodePorts)

  override def asMaster(): Unit = {
    out(addr)
    in(data)
  }

  def << (that: ArfQueryBus): Unit = {
    this.addr := that.addr
    that.data := this.data
  }
}

case class ArfCommitBus(commitPorts: Int) extends Bundle with IMasterSlave {
  val valid = Vec(Bool(), commitPorts)
  val addr = Vec(UInt(log2Up(32) bits), commitPorts)
  val data = Vec(UInt(32 bits), commitPorts)

  override def asMaster(): Unit = {
    out(valid, addr, data)
  }

  def << (that: ArfCommitBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
    this.data := that.data
  }
}
class ARF(decodePorts: Int) extends Component {
  val io = new Bundle {
    val queryPorts = slave(ArfQueryBus(decodePorts))
    val commitPorts = slave(ArfCommitBus(decodePorts))
  }
  noIoPrefix()

  import io._

  val rf = Vec(RegInit(U(0, 32 bits)), 32)

  for (i <- 0 until decodePorts) {
    for (j <- 0 until 2) {
      queryPorts.data(i)(j) := rf(queryPorts.addr(i)(j))
    }
  }

  when(commitPorts.valid(0) && commitPorts.valid(1) && commitPorts.addr(0) === commitPorts.addr(1)) {
    rf(commitPorts.addr(1)) := commitPorts.data(1)
  }.otherwise {
    when(commitPorts.valid(0)) {
      rf(commitPorts.addr(0)) := commitPorts.data(0)
    }
    when(commitPorts.valid(1)) {
      rf(commitPorts.addr(1)) := commitPorts.data(1)
    }
  }
}

object ARFVerilog extends App {
  Config.spinal.generateVerilog(new ARF(2))
}