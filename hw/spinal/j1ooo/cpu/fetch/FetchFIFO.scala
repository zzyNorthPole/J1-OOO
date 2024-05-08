package j1ooo.cpu.fetch

import j1ooo.cpu.blackbox.RamParameter
import j1ooo.cpu.fetch.bpu.RasItem
import j1ooo.cpu.signal.Exception
import j1ooo.cpu.util.Ram
import j1ooo.gen.{BpuConfig, Config, FetchFIFOConfig}
import spinal.core._
import spinal.lib.{Counter, StreamFifo, master, slave}

class FetchFIFO(bpuConfig: BpuConfig, fetchPorts: Int, fetchFIFOConfig: FetchFIFOConfig, _type: String) extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val input = slave(FetchOutputBus(bpuConfig, fetchPorts))
    val output = master(FetchOutputBus(bpuConfig, fetchPorts))
  }
  noIoPrefix()

  import io._

  val front = Counter(fetchFIFOConfig.lines)
  val end = Counter(fetchFIFOConfig.lines)

  input.ready := (end.asSInt + 1) =/= front.asSInt
  output.valid := front.asSInt =/= end.asSInt

  when(flush) {
    front.clear()
    end.clear()
  }.otherwise {
    when(input.ready && input.valid) {
      end.increment()
    }
    when(output.valid && output.ready) {
      front.increment()
    }
  }

  val fifo = new Area {
    val pcRams = Seq.fill(fetchPorts) {
      Ram(_type, RamParameter(
        fetchFIFOConfig.lines, 32, false, "dpdistram"
      ))
    }
    for (i <- 0 until fetchPorts) {
      pcRams(i).io.ena := input.valid && input.ready
      pcRams(i).io.wea := B(1, 1 bits)
      pcRams(i).io.addra := end.asSInt.asUInt
      pcRams(i).io.dina := input.dout.pc(i)

      pcRams(i).io.enb := output.valid && output.ready
      pcRams(i).io.addrb := front.asSInt.asUInt
      output.dout.pc(i) := pcRams(i).io.doutb
    }

    val instRams = Seq.fill(fetchPorts) {
      Ram(_type, RamParameter(
        fetchFIFOConfig.lines, 32, false, "dpdistram"
      ))
    }
    for (i <- 0 until fetchPorts) {
      instRams(i).io.ena := input.valid && input.ready
      instRams(i).io.wea := B(1, 1 bits)
      instRams(i).io.addra := end.asSInt.asUInt
      instRams(i).io.dina := input.dout.inst(i)

      instRams(i).io.enb := output.valid && output.ready
      instRams(i).io.addrb := front.asSInt.asUInt
      output.dout.inst(i) := instRams(i).io.doutb
    }

    val predictPcRams = Seq.fill(fetchPorts) {
      Ram(_type, RamParameter(
        fetchFIFOConfig.lines, 32, false, "dpdistram"
      ))
    }
    for (i <- 0 until fetchPorts) {
      predictPcRams(i).io.ena := input.valid && input.ready
      predictPcRams(i).io.wea := B(1, 1 bits)
      predictPcRams(i).io.addra := end.asSInt.asUInt
      predictPcRams(i).io.dina := input.dout.predict.predictPc(i)

      predictPcRams(i).io.enb := output.valid && output.ready
      predictPcRams(i).io.addrb := front.asSInt.asUInt
      output.dout.predict.predictPc(i) := predictPcRams(i).io.doutb
    }

    val historyRams = Seq.fill(fetchPorts) {
      Ram(_type, RamParameter(
        fetchFIFOConfig.lines, bpuConfig.bhtConfig.histories, false, "dpdistram"
      ))
    }
    for (i <- 0 until fetchPorts) {
      historyRams(i).io.ena := input.valid && input.ready
      historyRams(i).io.wea := B(1, 1 bits)
      historyRams(i).io.addra := end.asSInt.asUInt
      historyRams(i).io.dina := input.dout.predict.history(i)

      historyRams(i).io.enb := output.valid && output.ready
      historyRams(i).io.addrb := front.asSInt.asUInt
      output.dout.predict.history(i) := historyRams(i).io.doutb
    }

    val countRams = Seq.fill(fetchPorts) {
      Ram(_type, RamParameter(
        fetchFIFOConfig.lines, 2, false, "dpdistram"
      ))
    }
    for (i <- 0 until fetchPorts) {
      countRams(i).io.ena := input.valid && input.ready
      countRams(i).io.wea := B(1, 1 bits)
      countRams(i).io.addra := end.asSInt.asUInt
      countRams(i).io.dina := input.dout.predict.count(i)

      countRams(i).io.enb := output.valid && output.ready
      countRams(i).io.addrb := front.asSInt.asUInt
      output.dout.predict.count(i) := countRams(i).io.doutb
    }

    val curRasRams = Ram(_type, RamParameter(
      fetchFIFOConfig.lines, RasItem(bpuConfig.rasConfig).getBitsWidth, false, "dpdistram"
    ))
    curRasRams.io.ena := input.valid && input.ready
    curRasRams.io.wea := B(1, 1 bits)
    curRasRams.io.addra := end.asSInt.asUInt
    val curRasRamsDin = UInt(bpuConfig.rasConfig.width * bpuConfig.rasConfig.lines + log2Up(bpuConfig.rasConfig.lines) bits)
    for (i <- 0 until bpuConfig.rasConfig.lines) {
      curRasRamsDin(i * bpuConfig.rasConfig.width, bpuConfig.rasConfig.width bits) := input.dout.predict.curRas.stack(i)
    }
    curRasRamsDin(bpuConfig.rasConfig.lines * bpuConfig.rasConfig.width, log2Up(bpuConfig.rasConfig.lines) bits) := input.dout.predict.curRas.top
    curRasRams.io.dina := curRasRamsDin
    curRasRams.io.enb := output.valid && output.ready
    curRasRams.io.addrb := front.asSInt.asUInt
    val curRasRamsDout = curRasRams.io.doutb
    for (i <- 0 until bpuConfig.rasConfig.lines) {
      output.dout.predict.curRas.stack(i) := curRasRamsDout(i * bpuConfig.rasConfig.width, bpuConfig.rasConfig.width bits)
    }
    output.dout.predict.curRas.top := curRasRamsDout(bpuConfig.rasConfig.lines * bpuConfig.rasConfig.width, log2Up(bpuConfig.rasConfig.lines) bits)

    val exRams = Seq.fill(fetchPorts) {
      Ram(_type, RamParameter(
        fetchFIFOConfig.lines, FetchException().getBitsWidth, false, "dpdistram"
      ))
    }
    for (i <- 0 until fetchPorts) {
      exRams(i).io.ena := input.valid && input.ready
      exRams(i).io.wea := B(1, 1 bits)
      exRams(i).io.addra := end.asSInt.asUInt
      exRams(i).io.dina := input.dout.exDescription(i).valid.asUInt @@ input.dout.exDescription(i).op.asBits.asUInt @@ input.dout.exDescription(i).isTlbHit.asUInt

      exRams(i).io.enb := output.valid && output.ready
      exRams(i).io.addrb := front.asSInt.asUInt
      val res = exRams(i).io.doutb
      output.dout.exDescription(i).valid := res(FetchException().getBitsWidth - 1)
      output.dout.exDescription(i).op := res(FetchException().getBitsWidth - 2 downto 1).as(Exception())
      output.dout.exDescription(i).isTlbHit := res(0)
    }
  }
}

object FetchFIFOVerilog extends App {
  Config.spinal.generateVerilog(new FetchFIFO(Config.bpuConfig, 2, Config.fetchFIFOConfig, "sim"))
}