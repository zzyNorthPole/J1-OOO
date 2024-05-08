package j1ooo.cpu.execute

import j1ooo.cpu.blackbox.RamParameter
import j1ooo.cpu.decode.Cp0CtrlBus
import j1ooo.cpu.execute.memory.MSHRToCommitBus
import j1ooo.cpu.fetch.FetchException
import j1ooo.cpu.fetch.bpu.{PredictRes, RasItem}
import j1ooo.cpu.signal
import j1ooo.cpu.signal.{Cp0Reg, DispatchResult, Exception, InstType, JuOp, MduOp, TlbOp}
import j1ooo.cpu.util.Ram
import j1ooo.gen.{BpuConfig, Config, RobConfig}
import spinal.core._
import spinal.lib.{Counter, IMasterSlave, MuxOH, dataCarrierFragmentPimped, master, slave}

case class RobItemBase(robConfig: RobConfig, bpuConfig: BpuConfig) extends Bundle {
  val pc = UInt(32 bits)
  val instType = InstType()
  val archDest = new Bundle {
    val valid = Bool()
    val addr = UInt(log2Up(32) bits)
  }
  val predict = new Bundle {
    val history = UInt(bpuConfig.bhtConfig.histories bits)
    val count = UInt(2 bits)
  }
  val juOp = JuOp()
  val rs = UInt(5 bits)
  val tlbOp = TlbOp()
  val cp0 = Cp0CtrlBus()
  val exBeforeExecute = FetchException()
  val finish = Bool()
  val dispatchResult = DispatchResult()
}

case class RobBase(robConfig: RobConfig, bpuConfig: BpuConfig, decodePorts: Int) extends Bundle {
  val pc = Vec(UInt(32 bits), decodePorts)
  val instType = Vec(InstType(), decodePorts)
  // val phyDestAddr = Vec(UInt(log2Up(robConfig.lines) bits), decodePorts)
  val archDest = Vec(new Bundle {
    val valid = Bool()
    val addr = UInt(log2Up(32) bits)
  }, decodePorts)
  val predict = PredictRes(bpuConfig, decodePorts)
  val juOp = Vec(JuOp(), decodePorts)
  val rs = Vec(UInt(5 bits), decodePorts)
  val tlbOp = Vec(TlbOp(), decodePorts)
  val cp0 = Vec(Cp0CtrlBus(), decodePorts)
  val exDescription = Vec(FetchException(), decodePorts)
  val finish = Vec(Bool(), decodePorts)
  val dispatchResult = Vec(DispatchResult(), decodePorts)
}

case class RobPushBus(robConfig: RobConfig, bpuConfig: BpuConfig, decodePorts: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val payload = RobBase(robConfig, bpuConfig, decodePorts)
  val end = UInt(log2Up(robConfig.lines / decodePorts) bits)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready, end)
  }

  def << (that: RobPushBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
    that.ready := this.ready
    that.end := this.end
  }
}

case class RobQueryBus(robConfig: RobConfig, decodePorts: Int) extends Bundle with IMasterSlave {
  val addr = Vec(Vec(UInt(log2Up(robConfig.lines) bits), 2), decodePorts)
  val finish = Vec(Vec(Bool(), 2), decodePorts)
  val data = Vec(Vec(UInt(32 bits), 2), decodePorts)

  override def asMaster(): Unit = {
    out(addr)
    in(finish, data)
  }

  def << (that: RobQueryBus): Unit = {
    this.addr := that.addr
    that.finish := this.finish
    that.data := this.data
  }
}

case class RobNormalItem() extends Bundle {
  val dout = UInt(32 bits)

  val movFail = Bool()

  val predictRes = new Bundle {
    val fail = Bool()
    val nextPc = UInt(32 bits)
  }

  val bpu = new Bundle {
    val nextHistory = Bool()
    val targetPc = UInt(32 bits)
  }

  val exception = new Bundle {
    val valid = Bool()
    val op = Exception()
  }
}

case class RobNormalWriteBackBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(log2Up(robConfig.lines) bits)
  val payload = RobNormalItem()

  override def asMaster(): Unit = {
    out(valid, addr, payload)
  }

  def << (that: RobNormalWriteBackBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
    this.payload := that.payload
  }
}

case class RobMuldivItem() extends Bundle {
  val op = MduOp()
  val din1 = UInt(32 bits)
  val hiloDin = Vec(UInt(32 bits), 2)
  val dout = UInt(32 bits)
}

case class RobMuldivWriteBackBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(log2Up(robConfig.lines) bits)
  val payload = RobMuldivItem()

  override def asMaster(): Unit = {
    out(valid, addr, payload)
  }

  def << (that: RobMuldivWriteBackBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
    this.payload := that.payload
  }
}

case class RobMemoryItem() extends Bundle {
  val addr = UInt(32 bits)
  val isCached = Bool()
  val exception = FetchException()
}

case class RobMemoryWriteBackBus(robConfig: RobConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(log2Up(robConfig.lines) bits)
  val complete = Bool()
  val payload = RobMemoryItem()
  val dout = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, addr, complete, payload, dout)
  }

  def << (that: RobMemoryWriteBackBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
    this.complete := that.complete
    this.payload := that.payload
    this.dout := that.dout
  }
}

case class RobCommitItem(robConfig: RobConfig, bpuConfig: BpuConfig) extends Bundle {
  val pc = UInt(32 bits)
  val instType = InstType()
  val finish = Bool()
  // PRF
  val phyDest = UInt(log2Up(robConfig.lines) bits)
  // ARF
  val archDest = new Bundle {
    val valid = Bool()
    val addr = UInt(log2Up(32) bits)
    val dout = UInt(32 bits)
  }
  val movFail = Bool()
  // JU
  val predict = new Bundle {
    val op = JuOp()
    val rs = UInt(5 bits)
    val targetPc = UInt(32 bits)
    val history = UInt(bpuConfig.bhtConfig.histories bits)
    val count = UInt(2 bits)
    val curRas = RasItem(bpuConfig.rasConfig)
  }
  val predictRes = new Bundle {
    val fail = Bool()
    val nextPc = UInt(32 bits)
  }
  // HI LO
  val mduOp = MduOp()
  val hiloDin = Vec(UInt(32 bits), 2)
  // TLB
  val tlbOp = TlbOp()
  // CP0
  val mtc0 = new Bundle {
    val addr = Cp0Reg()
    val select = UInt(3 bits)
    val din1 = UInt(32 bits)
  }
  // MSHR
  val complete = Bool()
  // storebuffer
  val isCached = Bool()
  // ICACHE
  val addr = UInt(32 bits)
  val exDescription = FetchException()
  val exBadAddr = UInt(32 bits)
}

case class RobCommitBus(robConfig: RobConfig, bpuConfig: BpuConfig, commitPorts: Int) extends Bundle with IMasterSlave {
  val valid = Vec(Bool(), commitPorts)
  val ready = Vec(Bool(), commitPorts)
  val payload = Vec(RobCommitItem(robConfig, bpuConfig), commitPorts)

  override def asMaster(): Unit = {
    out(valid, payload)
    in(ready)
  }

  def << (that: RobCommitBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
    that.ready := this.ready
  }
}

class ROB(robConfig: RobConfig, bpuConfig: BpuConfig, decodePorts: Int) extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val wPorts = slave(RobPushBus(robConfig, bpuConfig, decodePorts))
    val queryPorts = slave(RobQueryBus(robConfig, decodePorts))
    val normalWriteBackBus = Vec(slave(RobNormalWriteBackBus(robConfig)), 2)
    val muldivWriteBackBus = slave(RobMuldivWriteBackBus(robConfig))
    val memoryWriteBackBus = slave(RobMemoryWriteBackBus(robConfig))
    val mshrWriteBackBus = master(MSHRToCommitBus(robConfig))
    val commitPorts = master(RobCommitBus(robConfig, bpuConfig, decodePorts))
  }
  noIoPrefix()

  import io._

  val front = Counter(robConfig.lines / decodePorts)
  val end = Counter(robConfig.lines / decodePorts)
  val bias = RegInit(False)

  when(flush) {
    front.clear()
    end.clear()
    bias.clear()
  }

  wPorts.ready := (end.asSInt.asUInt + 1) =/= front.asSInt.asUInt
  wPorts.end := end

  when(!flush && wPorts.valid && wPorts.ready) {
    end.increment()
  }

  when(!flush) {
    when(commitPorts.valid(1) && commitPorts.ready(1)) {
      front.increment()
    }.elsewhen(commitPorts.valid(0) && commitPorts.ready(0)) {
      when(bias) {
        front.increment()
        bias.clear()
      }.otherwise {
        bias.set()
      }
    }
  }

  val fifo = new Area {
    val baseRams = Seq.fill(decodePorts) {
      Mem(RobItemBase(robConfig, bpuConfig), robConfig.lines / decodePorts)
    }
    val curRasRam = Mem(RasItem(bpuConfig.rasConfig), robConfig.lines / decodePorts)
    for (i <- 0 until decodePorts) {
      val curBase = RobItemBase(robConfig, bpuConfig)
      curBase.pc := wPorts.payload.pc(i)
      curBase.instType := wPorts.payload.instType(i)
      curBase.archDest.valid := wPorts.payload.archDest(i).valid
      curBase.archDest.addr := wPorts.payload.archDest(i).addr
      curBase.predict.history := wPorts.payload.predict.history(i)
      curBase.predict.count := wPorts.payload.predict.count(i)
      curBase.juOp := wPorts.payload.juOp(i)
      curBase.cp0 := wPorts.payload.cp0(i)
      curBase.rs := wPorts.payload.rs(i)
      curBase.tlbOp := wPorts.payload.tlbOp(i)
      curBase.exBeforeExecute := wPorts.payload.exDescription(i)
      curBase.finish := wPorts.payload.finish(i)
      curBase.dispatchResult := wPorts.payload.dispatchResult(i)
      baseRams(i).write(
        address = end,
        data = curBase,
        enable = !flush && wPorts.valid && wPorts.ready
      )
    }
    curRasRam.write(
      address = end,
      data = wPorts.payload.predict.curRas,
      enable = !flush && wPorts.valid && wPorts.ready
    )
    val renameQueryFinishBeforeDispatch = Vec(Vec(Bool(), 2), decodePorts)
    val renameQueryDispatchResult = Vec(Vec(DispatchResult(), 2), decodePorts)
    for (i <- 0 until decodePorts) {
      for (j <- 0 until 2) {
        val curRes = queryPorts.addr(i)(j)(0).mux(
          False -> baseRams(0).readAsync(
            address = queryPorts.addr(i)(j)(1, (log2Up(robConfig.lines) - 1) bits)
          ),
          True -> baseRams(1).readAsync(
            address = queryPorts.addr(i)(j)(1, (log2Up(robConfig.lines) - 1) bits)
          )
        )
        renameQueryFinishBeforeDispatch(i)(j) := curRes.finish
        renameQueryDispatchResult(i)(j) := curRes.dispatchResult
      }
    }
    val commitQueryBase = Vec(RobItemBase(robConfig, bpuConfig), 2)
    commitQueryBase(0) := baseRams(0).readAsync(
      address = bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt
    )
    commitQueryBase(1) := baseRams(1).readAsync(
      address = front
    )
    val commitCurRas = Vec(RasItem(bpuConfig.rasConfig), 2)
    commitCurRas(0) := curRasRam.readAsync(
      address = bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt
    )
    commitCurRas(1) := curRasRam.readAsync(
      address = front
    )

    val normalFinish = Vec(RegInit(U(0, (robConfig.lines / decodePorts) bits)), decodePorts * 2)
    for (i <- 0 until 2) {
      for (j <- 0 until decodePorts) {
        when(!flush && wPorts.valid && wPorts.ready) {
          normalFinish(i * decodePorts + j)(end).clear()
        }
        when(!flush && normalWriteBackBus(i).valid && normalWriteBackBus(i).addr(0, log2Up(decodePorts) bits) === U(j, log2Up(decodePorts) bits)) {
          normalFinish(i * decodePorts + j)(normalWriteBackBus(i).addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)) := True
        }
      }
    }
    val renameQueryFinishNormal = Vec(Vec(Vec(Bool(), 2), decodePorts), 2)
    for (i <- 0 until 2) {
      for (j <- 0 until decodePorts) {
        for (k <- 0 until 2) {
          renameQueryFinishNormal(i)(j)(k) := queryPorts.addr(j)(k)(0).mux(
            False -> normalFinish(i * decodePorts + 0)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)),
            True -> normalFinish(i * decodePorts + 1)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits))
          )
        }
      }
    }
    val commitNormalFinish = Vec(Vec(Bool(), 2), 2)
    for (i <- 0 until 2) {
      commitNormalFinish(i)(0) := normalFinish(i * decodePorts + 0)(bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt)
      commitNormalFinish(i)(1) := normalFinish(i * decodePorts + 1)(front)
    }

    val normalResultRams = Seq.fill(decodePorts * 2) {
      Mem(RobNormalItem(), robConfig.lines / decodePorts)
    }
    for (i <- 0 until 2) {
      for (j <- 0 until decodePorts) {
        normalResultRams(i * decodePorts + j).write(
          address = normalWriteBackBus(i).addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits),
          data = normalWriteBackBus(i).payload,
          enable = !flush && normalWriteBackBus(i).valid && normalWriteBackBus(i).addr(0, log2Up(decodePorts) bits) === U(j, log2Up(decodePorts) bits)
        )
      }
    }
    val renameQueryDataNormal = Vec(Vec(Vec(UInt(32 bits), 2), decodePorts), 2)
    for (i <- 0 until 2) {
      for (j <- 0 until decodePorts) {
        for (k <- 0 until 2) {
          renameQueryDataNormal(i)(j)(k) := queryPorts.addr(j)(k)(0).mux(
            False -> normalResultRams(i * decodePorts + 0).readAsync(
              address = queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)
            ).dout,
            True -> normalResultRams(i * decodePorts + 1).readAsync(
              address = queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)
            ).dout
          )
        }
      }
    }
    val commitNormalResult = Vec(Vec(RobNormalItem(), 2), 2)
    for (i <- 0 until 2) {
      commitNormalResult(i)(0) := normalResultRams(i * decodePorts + 0).readAsync(
        address = bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt
      )
      commitNormalResult(i)(1) := normalResultRams(i * decodePorts + 1).readAsync(
        address = front
      )
    }

    val muldivFinish = Vec(RegInit(U(0, (robConfig.lines / decodePorts) bits)), decodePorts)
    for (i <- 0 until decodePorts) {
      when(!flush && wPorts.valid && wPorts.ready) {
        muldivFinish(i)(end).clear()
      }
      when(!flush && muldivWriteBackBus.valid && muldivWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)) {
        muldivFinish(i)(muldivWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)).set()
      }
    }
    val renameQueryFinishMuldiv = Vec(Vec(Bool(), 2), decodePorts)
    for (j <- 0 until decodePorts) {
      for (k <- 0 until 2) {
        renameQueryFinishMuldiv(j)(k) := queryPorts.addr(j)(k)(0).mux(
          False -> muldivFinish(0)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)),
          True -> muldivFinish(1)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits))
        )
      }
    }
    val commitMuldivFinish = Vec(Bool(), 2)
    commitMuldivFinish(0) := muldivFinish(0)(bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt)
    commitMuldivFinish(1) := muldivFinish(1)(front)

    val muldivResultRams = Seq.fill(decodePorts) {
      Mem(RobMuldivItem(), robConfig.lines / decodePorts)
    }
    for (i <- 0 until decodePorts) {
      muldivResultRams(i).write(
        address = muldivWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits),
        data = muldivWriteBackBus.payload,
        enable = !flush && muldivWriteBackBus.valid && muldivWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)
      )
    }
    val renameQueryDataMuldiv = Vec(Vec(UInt(32 bits), 2), decodePorts)
    for (j <- 0 until decodePorts) {
      for (k <- 0 until 2) {
        renameQueryDataMuldiv(j)(k) := queryPorts.addr(j)(k)(0).mux(
          False -> muldivResultRams(0).readAsync(
            address = queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)
          ).dout,
          True -> muldivResultRams(1).readAsync(
            address = queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)
          ).dout
        )
      }
    }
    val commitMuldivResult = Vec(RobMuldivItem(), 2)
    commitMuldivResult(0) := muldivResultRams(0).readAsync(
      address = bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt
    )
    commitMuldivResult(1) := muldivResultRams(1).readAsync(
      address = front
    )

    val memoryFinish = Vec(RegInit(U(0, (robConfig.lines / decodePorts) bits)), decodePorts)
    for (i <- 0 until decodePorts) {
      when(!flush && wPorts.valid && wPorts.ready) {
        memoryFinish(i)(end).clear()
      }
      when(!flush && memoryWriteBackBus.valid && memoryWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)) {
        memoryFinish(i)(memoryWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)).set()
      }
    }
    val renameQueryFinishMemory = Vec(Vec(Bool(), 2), decodePorts)
    for (j <- 0 until decodePorts) {
      for (k <- 0 until 2) {
        renameQueryFinishMemory(j)(k) := queryPorts.addr(j)(k)(0).mux(
          False -> memoryFinish(0)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)),
          True -> memoryFinish(1)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits))
        )
      }
    }
    val commitMemoryFinish = Vec(Bool(), 2)
    commitMemoryFinish(0) := memoryFinish(0)(bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt)
    commitMemoryFinish(1) := memoryFinish(1)(front)

    mshrWriteBackBus.valid.clear()
    val memoryComplete = Vec(RegInit(U(0, (robConfig.lines / decodePorts) bits)), decodePorts)
    for (i <- 0 until decodePorts) {
      when(!flush && wPorts.valid && wPorts.ready) {
        memoryComplete(i)(end).clear()
      }
      when(!flush && memoryWriteBackBus.valid && memoryWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)) {
        memoryComplete(i)(memoryWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)) := memoryWriteBackBus.complete
      }
      when(!flush && mshrWriteBackBus.ready && mshrWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)) {
        memoryComplete(i)(mshrWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)).set()
      }
    }
    val renameQueryCompleteMemory = Vec(Vec(Bool(), 2), decodePorts)
    for (j <- 0 until decodePorts) {
      for (k <- 0 until 2) {
        renameQueryCompleteMemory(j)(k) := queryPorts.addr(j)(k)(0).mux(
          False -> memoryComplete(0)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)),
          True -> memoryComplete(1)(queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits))
        )
      }
    }
    val commitMemoryComplete = Vec(Bool(), 2)
    commitMemoryComplete(0) := memoryComplete(0)(bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt)
    commitMemoryComplete(1) := memoryComplete(1)(front)

    val memoryResultRams = Seq.fill(decodePorts) {
      Mem(RobMemoryItem(), robConfig.lines / decodePorts)
    }
    val memoryDoutRams = Seq.fill(decodePorts) {
      Mem(UInt(32 bits), robConfig.lines / decodePorts)
    }
    for (i <- 0 until decodePorts) {
      memoryResultRams(i).write(
        address = memoryWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits),
        data = memoryWriteBackBus.payload,
        enable = !flush && memoryWriteBackBus.valid && memoryWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)
      )
      memoryDoutRams(i).write(
        address = memoryWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits),
        data = memoryWriteBackBus.dout,
        enable = !flush && memoryWriteBackBus.valid && memoryWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)
      )
    }
    for (i <- 0 until decodePorts) {
      memoryDoutRams(i).write(
        address = mshrWriteBackBus.addr(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits),
        data = mshrWriteBackBus.data,
        enable = !flush && mshrWriteBackBus.ready && mshrWriteBackBus.addr(0, log2Up(decodePorts) bits) === U(i, log2Up(decodePorts) bits)
      )
    }
    val renameQueryDataMemory = Vec(Vec(UInt(32 bits), 2), decodePorts)
    for (j <- 0 until decodePorts) {
      for (k <- 0 until 2) {
        renameQueryDataMemory(j)(k) := queryPorts.addr(j)(k)(0).mux(
          False -> memoryDoutRams(0).readAsync(
            address = queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)
          ),
          True -> memoryDoutRams(1).readAsync(
            address = queryPorts.addr(j)(k)(log2Up(decodePorts), (log2Up(robConfig.lines) - log2Up(decodePorts)) bits)
          )
        )
      }
    }
    val commitMemoryResult = Vec(RobMemoryItem(), 2)
    commitMemoryResult(0) := memoryResultRams(0)(bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt)
    commitMemoryResult(1) := memoryResultRams(1)(front)
    val commitMemoryDout = Vec(UInt(32 bits), 2)
    commitMemoryDout(0) := memoryDoutRams(0)(bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt)
    commitMemoryDout(1) := memoryDoutRams(1)(front)

    for (i <- 0 until decodePorts) {
      for (j <- 0 until 2) {
        queryPorts.finish(i)(j) := renameQueryDispatchResult(i)(j).mux(
          DispatchResult.NORMAL0 -> renameQueryFinishNormal(0)(i)(j),
          DispatchResult.NORMAL1 -> renameQueryFinishNormal(1)(i)(j),
          DispatchResult.MULDIV -> renameQueryFinishMuldiv(i)(j),
          DispatchResult.MEMORY -> (renameQueryFinishMemory(i)(j) && renameQueryCompleteMemory(i)(j)),
          DispatchResult.NONE -> renameQueryFinishBeforeDispatch(i)(j)
        )

        queryPorts.data(i)(j) := renameQueryDispatchResult(i)(j).mux(
          DispatchResult.NORMAL0 -> renameQueryDataNormal(0)(i)(j),
          DispatchResult.NORMAL1 -> renameQueryDataNormal(1)(i)(j),
          DispatchResult.MULDIV -> renameQueryDataMuldiv(i)(j),
          DispatchResult.MEMORY -> renameQueryDataMemory(i)(j),
          DispatchResult.NONE -> U(0, 32 bits)
        )
      }
    }

    val commitResult = Vec(RobCommitItem(robConfig, bpuConfig), 2)
    for (i <- 0 until decodePorts) {
      commitResult(i).pc := commitQueryBase(i).pc
      commitResult(i).instType := commitQueryBase(i).instType
      commitResult(i).finish := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalFinish(0)(i),
        DispatchResult.NORMAL1 -> commitNormalFinish(1)(i),
        DispatchResult.MULDIV -> commitMuldivFinish(i),
        DispatchResult.MEMORY -> commitMemoryFinish(i),
        DispatchResult.NONE -> commitQueryBase(i).finish
      )
      if (i == 0) commitResult(i).phyDest := (bias ? (front.asSInt.asUInt + 1) | front.asSInt.asUInt) @@ U(0, 1 bits)
      else commitResult(i).phyDest := front @@ U(1, 1 bits)
      commitResult(i).archDest.valid := commitQueryBase(i).archDest.valid
      commitResult(i).archDest.addr := commitQueryBase(i).archDest.addr
      commitResult(i).archDest.dout := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalResult(0)(i).dout,
        DispatchResult.NORMAL1 -> commitNormalResult(1)(i).dout,
        DispatchResult.MULDIV -> commitMuldivResult(i).dout,
        DispatchResult.MEMORY -> commitMemoryDout(i),
        DispatchResult.NONE -> U(0, 32 bits)
      )
      commitResult(i).movFail := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalResult(0)(i).movFail,
        DispatchResult.NORMAL1 -> commitNormalResult(1)(i).movFail,
        default -> False
      )
      commitResult(i).predict.targetPc := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalResult(0)(i).bpu.targetPc,
        default -> commitNormalResult(1)(i).bpu.targetPc
      )
      commitResult(i).predict.history := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitQueryBase(i).predict.history,
        default -> commitQueryBase(i).predict.history
      )
      commitResult(i).predict.count := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitQueryBase(i).predict.count,
        default -> commitQueryBase(i).predict.count
      )
      commitResult(i).predict.curRas := commitCurRas(i)
      commitResult(i).predictRes.fail := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalResult(0)(i).predictRes.fail,
        default -> commitNormalResult(1)(i).predictRes.fail
      )
      commitResult(i).predictRes.nextPc := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalResult(0)(i).predictRes.nextPc,
        default -> commitNormalResult(1)(i).predictRes.nextPc
      )
      commitResult(i).mduOp := commitMuldivResult(i).op
      commitResult(i).hiloDin := commitMuldivResult(i).hiloDin
      commitResult(i).mtc0.addr := commitQueryBase(i).cp0.addr
      commitResult(i).mtc0.select := commitQueryBase(i).cp0.select
      commitResult(i).mtc0.din1 := commitMuldivResult(i).din1
      commitResult(i).predict.op := commitQueryBase(i).juOp
      commitResult(i).predict.rs := commitQueryBase(i).rs
      commitResult(i).tlbOp := commitQueryBase(i).tlbOp
      commitResult(i).complete := commitMemoryComplete(i)
      commitResult(i).isCached := commitMemoryResult(i).isCached
      commitResult(i).addr := commitMemoryResult(i).addr
      commitResult(i).exDescription.valid := commitQueryBase(i).dispatchResult.mux(
        DispatchResult.NORMAL0 -> commitNormalResult(0)(i).exception.valid,
        DispatchResult.NORMAL1 -> commitNormalResult(1)(i).exception.valid,
        DispatchResult.MULDIV -> False,
        DispatchResult.MEMORY -> commitMemoryResult(i).exception.valid,
        DispatchResult.NONE -> commitQueryBase(i).exBeforeExecute.valid
      )
      commitResult(i).exDescription.op := MuxOH(
        Vec(
          commitQueryBase(i).exBeforeExecute.valid,
          commitNormalResult(0)(i).exception.valid,
          commitNormalResult(1)(i).exception.valid,
          commitMemoryResult(i).exception.valid
        ),
        Vec(
          commitQueryBase(i).exBeforeExecute.op,
          commitNormalResult(0)(i).exception.op,
          commitNormalResult(1)(i).exception.op,
          commitMemoryResult(i).exception.op
        )
      )
      commitResult(i).exDescription.isTlbHit := (
        commitQueryBase(i).exBeforeExecute.valid ?
          commitQueryBase(i).exBeforeExecute.isTlbHit |
          commitMemoryResult(i).exception.isTlbHit
      )
      commitResult(i).exBadAddr := (
        commitQueryBase(i).exBeforeExecute.valid ?
          commitQueryBase(i).pc |
          commitMemoryResult(i).addr
      )
    }
    commitPorts.valid(0) := front =/= end
    commitPorts.valid(1) := bias ? (front.asSInt + 1 =/= end.asSInt) | (front =/= end)
    commitPorts.payload(0) := bias ? commitResult(1) | commitResult(0)
    commitPorts.payload(1) := bias ? commitResult(0) | commitResult(1)
  }
}

object RobVerilog extends App {
  Config.spinal.generateVerilog(new ROB(robConfig = Config.robConfig, bpuConfig = Config.bpuConfig, decodePorts = 2))
}