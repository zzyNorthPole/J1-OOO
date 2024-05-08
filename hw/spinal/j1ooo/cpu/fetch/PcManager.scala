package j1ooo.cpu.fetch

import spinal.core._

class PcManager(fetchPortsNum: Int) extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val flushPc = in UInt(32 bits)

    val ready = in Bool()
    val pc = out Vec(UInt(32 bits), fetchPortsNum)
    val addrErrorLoad = out Vec(Bool(), fetchPortsNum)
    val nextPc = in Vec(UInt(32 bits), fetchPortsNum)
  }
  noIoPrefix()
  import io._

  val pcReg = Vec(Reg(UInt(32 bits)), fetchPortsNum)
  pcReg(0) init U"32'hBFC00000"
  pcReg(1) init U"32'hBFC00004"

  when(flush) {
    pcReg(0) := flushPc
    pcReg(1) := flushPc + 4
  }.elsewhen(ready) {
    for (i <- 0 until fetchPortsNum) {
      pcReg(i) := nextPc(i)
    }
  }

  for (i <- 0 until fetchPortsNum) {
    pc(i) := pcReg(i)
    addrErrorLoad(i) := (pc(i)(0, 2 bits) =/= U(0, 2 bits))
  }
}
