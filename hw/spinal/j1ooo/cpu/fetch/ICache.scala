package j1ooo.cpu.fetch

import j1ooo.cpu.blackbox.RamParameter
import j1ooo.cpu.util.{BusRename, Lfsr, Ram}
import j1ooo.gen.{CacheConfig, Config}
import j1ooo.gen.Config.axi4Config
import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4ReadOnly
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.misc.pipeline.{Builder, CtrlLink, Payload, StageLink}
import spinal.lib.{IMasterSlave, MuxOH, OHMasking, master, slave, traversableOnceBoolPimped}

case class ICacheFetchInputBus(fetchPortsNum: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val cached = Vec(Bool(), fetchPortsNum)
  val addr = Vec(UInt(32 bits), fetchPortsNum)
  val exception = Vec(Bool(), fetchPortsNum)

  override def asMaster(): Unit = {
    out(valid, cached, addr, exception)
  }
}

case class ICacheFetchOutputBus(fetchPortsNum: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val dout = Vec(UInt(32 bits), fetchPortsNum)

  override def asMaster(): Unit = {
    out(valid, ready, dout)
  }
}

case class ICacheOpInputBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(32 bits)

  override def asMaster(): Unit = {
    out(valid, addr)
  }

  def << (that: ICacheOpInputBus): Unit = {
    this.valid := that.valid
    this.addr := that.addr
  }
}

class ICache(cacheConfig: CacheConfig, fetchPortsNum: Int, _type: String) extends Component {
  val io = new Bundle {
    val flush = in Bool() // global

    val fetchPorts = new Bundle {
      val halt = in Bool()
      val input = slave(ICacheFetchInputBus(fetchPortsNum))
      val output = master(ICacheFetchOutputBus(fetchPortsNum))
    }

    val cacheOpPort = slave(ICacheOpInputBus())

    val ibus = master(Axi4ReadOnly(axi4Config)).setIdle() // if2
  }
  noIoPrefix()

  import io._
  BusRename.setName(ibus)

  val tagRams = Seq.fill(cacheConfig.ways) {
    Ram(_type, RamParameter(
      cacheConfig.lines, cacheConfig.tagWidth, false, "dpdistram"
    ))
  }

  val validRams = Seq.fill(cacheConfig.ways) {
    Ram("vec", RamParameter(
      cacheConfig.lines, 1, false, "dpdistram"
    ))
  }

  val dataRams = Seq.fill(cacheConfig.words) {
    Seq.fill(cacheConfig.ways) {
      Ram(_type, RamParameter(
        cacheConfig.lines, 32, true, "tdpram"
      ))
    }
  }

  val cacheOpAction = new Area {
    val index = UInt(cacheConfig.indexWidth bits)
    index := cacheOpPort.addr(cacheConfig.offsetWidth, cacheConfig.indexWidth bits)
  }

  val fetch1, fetch2 = CtrlLink()
  val fetch1To2 = StageLink(fetch1.down, fetch2.up)

  val fetch1Action = new fetch1.Area {
    fetch1.haltWhen(cacheOpPort.valid)
    fetch1.haltWhen(fetchPorts.halt)

    fetch1.throwWhen(
      flush | fetchPorts.input.exception.orR,
      usingReady = true
    )

    up.valid := fetchPorts.input.valid

    val CACHED = insert(io.fetchPorts.input.cached)
    val ADDR = insert(io.fetchPorts.input.addr)

    val TAG = insert(
      Vec(
        down(ADDR)(0)(32 - cacheConfig.tagWidth, cacheConfig.tagWidth bits),
        down(ADDR)(1)(32 - cacheConfig.tagWidth, cacheConfig.tagWidth bits)
      )
    )

    val INDEX = insert(
      Vec(
        down(ADDR)(0)(cacheConfig.offsetWidth, cacheConfig.indexWidth bits),
        down(ADDR)(1)(cacheConfig.offsetWidth, cacheConfig.indexWidth bits)
      )
    )

    val OFFSET = insert(
      Vec(
        down(ADDR)(0)(0, cacheConfig.offsetWidth bits),
        down(ADDR)(1)(0, cacheConfig.offsetWidth bits)
      )
    )

    val tags = Vec(Vec(UInt(cacheConfig.tagWidth bits), cacheConfig.ways), fetchPortsNum)
    for (i <- 0 until cacheConfig.ways) {
      tags(0)(i) := tagRams(i).io.douta
      tags(1)(i) := tagRams(i).io.doutb
    }

    val valids = Vec(Vec(Bool(), cacheConfig.ways), fetchPortsNum)
    for (i <- 0 until cacheConfig.ways) {
      valids(0)(i) := validRams(i).io.douta.asBool
      valids(1)(i) := validRams(i).io.doutb.asBool
    }

    val hits = Vec(Vec(Bool(), cacheConfig.ways), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      for (j <- 0 until cacheConfig.ways) {
        hits(i)(j) := valids(i)(j) && (tags(i)(j) === TAG(i))
      }
    }
    val hit = Vec(Bool(), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      hit(i) := hits(i).orR && down(CACHED)(i)
    }
    val HIT = insert(hit)
    val FETCH_SAME_LINE = insert((!hit(0) && down(CACHED)(0) && !hit(1) && down(CACHED)(1)) && down(TAG)(0) === down(TAG)(1) && down(INDEX)(0) === down(INDEX)(1))
    val hitWay = Vec(UInt(log2Up(cacheConfig.ways) bits), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      hitWay(i) := MuxOH(
        hits(i),
        for (j <- 0 until cacheConfig.ways) yield {
          U(j, log2Up(cacheConfig.ways) bits)
        }
      )
    }

    // invalid way select
    // combination
    val invalids = Vec(Vec(Bool(), cacheConfig.ways), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      for (j <- 0 until cacheConfig.ways) {
        invalids(i)(j) := ~valids(i)(j)
      }
    }
    val invalid = Vec(Bool(), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      invalid(i) := invalids(i).orR
    }
    val invalidWay = Vec(UInt(log2Up(cacheConfig.ways) bits), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      invalidWay(i) := MuxOH(
        OHMasking.first(invalids(i)),
        for (j <- 0 until cacheConfig.ways) yield {
          U(j, log2Up(cacheConfig.ways) bits)
        }
      )
    }

    // random replace way generation, sequential
    val lfsrWidth = log2Up(cacheConfig.ways) + 2
    val lfsr = new Lfsr(lfsrWidth)
    lfsr.io.en := up.isFiring
    lfsr.io.seed := U((lfsrWidth - 1 downto 0) -> true)
    // actually if2.lfsrDout, put it here in order to avoid recursive definition
    val lfsrDout = UInt(lfsrWidth bits)
    lfsrDout := lfsr.io.dout

    // replace way, select by invalid condition
    val replaceWay = Vec(UInt(log2Up(cacheConfig.ways) bits), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      replaceWay(i) := invalid(i) ? invalidWay(i) | lfsrDout(0, log2Up(cacheConfig.ways) bits)
    }

    val targetWay = Vec(UInt(log2Up(cacheConfig.ways) bits), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      targetWay(i) := hit(i) ? hitWay(i) | replaceWay(i)
    }
    val TARGET_WAY = insert(targetWay)
  }
  fetch1Action.setName("fetch1").reflectNames()

  val fetch2Action = new fetch2.Area {
    val dataLines = Vec(Vec(Vec(UInt(32 bits), cacheConfig.ways), cacheConfig.words), fetchPortsNum)
    for (i <- 0 until cacheConfig.words) {
      for (j <- 0 until cacheConfig.ways) {
        dataLines(0)(i)(j) := dataRams(i)(j).io.douta
        dataLines(1)(i)(j) := dataRams(i)(j).io.doutb
      }
    }

    val data = Vec(Vec(UInt(32 bits), cacheConfig.ways), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      for (j <- 0 until cacheConfig.ways) {
        data(i)(j) := dataLines(i)(up(fetch1Action.OFFSET)(i)((cacheConfig.offsetWidth - 1) downto 2))(j)
      }
    }

    val result = Vec(UInt(32 bits), fetchPortsNum)
    for (i <- 0 until fetchPortsNum) {
      result(i) := data(i)(up(fetch1Action.TARGET_WAY)(i))
    }
    val resultReg = Vec(RegInit(U(0, 32 bits)), fetchPortsNum)

    val ibusInit = new Area {
      import io.ibus._

      ar.addr := up(fetch1Action.HIT)(0) ? (
        up(fetch1Action.CACHED)(1) ? (up(fetch1Action.ADDR)(1)(31 downto cacheConfig.offsetWidth) @@ U(0, cacheConfig.offsetWidth bits)) | up(fetch1Action.ADDR)(1)
      ) | (
        up(fetch1Action.CACHED)(0) ? (up(fetch1Action.ADDR)(0)(31 downto cacheConfig.offsetWidth) @@ U(0, cacheConfig.offsetWidth bits)) | up(fetch1Action.ADDR)(0)
      )
      ar.id := U(0, 4 bits)
      ar.len := up(fetch1Action.HIT)(0) ? (
        up(fetch1Action.CACHED)(1) ? U(cacheConfig.words - 1, 8 bits) | U(0, 8 bits)
      ) | (
        up(fetch1Action.CACHED)(0) ? U(cacheConfig.words - 1, 8 bits) | U(0, 8 bits)
      )
      ar.size := U(2, 3 bits) // 4 bytes
      ar.burst := up(fetch1Action.HIT)(0) ? (
        up(fetch1Action.CACHED)(1) ? B(1, 2 bits) | B(0, 2 bits)
      ) | (
        up(fetch1Action.CACHED)(0) ? B(1, 2 bits) | B(0, 2 bits)
      ) // cached INCR uncached FIXED
      // ar.lock := B(0, 2 bits)
      ar.cache := B(0, 4 bits)
      ar.prot := B(0, 3 bits)
    }

    val fetch_same_line = Bool()
    fetch_same_line := up(fetch1Action.FETCH_SAME_LINE)
    val replaceWayDataReg = Vec(RegInit(U(0, 32 bits)), cacheConfig.words)
    val fix = RegInit(False)
    val count = RegInit(U(0, log2Up(cacheConfig.words) bits))
    val readNewValid, readNewReady = CombInit(False)
    val instReadNewFSM = new StateMachine {
      import io.ibus._

      setEntry(stateBoot)
      disableAutoStart()

      val readNewAr = new State()
      val readNewR = new State()

      stateBoot.whenIsActive {
        when(readNewValid) {
          goto(readNewAr)
        }
      }

      readNewAr.whenIsActive {
        ar.valid := True
        when(ar.ready) {
          count := 0
          goto(readNewR)
        }
      }

      readNewR.whenIsActive {
        r.ready := True
        when(r.valid) {
          replaceWayDataReg(count) := r.data.asUInt
          count := count + 1
          when(r.last) {
            readNewReady := True
            goto(stateBoot)
          }
        }
      }
    }
    val floodFillReady = CombInit(False)
    // FIXME: pipeline is not compatible to FSM!!!
    val instFetchFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val readNew = new State()
      val floodFill = new State()

      stateBoot.whenIsActive {
        when(up.valid && !flush && (!up(fetch1Action.HIT)(0) || !up(fetch1Action.HIT)(1))) {
          fix.set()
          // fetch2.haltIt()
          readNewValid.set()
          when(!fix) {
            resultReg := result
          }
          goto(readNew)
        }.otherwise {
          fix.clear()
        }
      }

      readNew.whenIsActive {
        // fetch2.haltIt()
        when(readNewReady) {
          goto(floodFill)
        }
      }

      floodFill.whenIsActive {
        // fetch2.haltIt()
        floodFillReady := !cacheOpPort.valid
        when(floodFillReady) {
          when(fetch_same_line) {
            for (i <- 0 until fetchPortsNum) {
              resultReg(i) := replaceWayDataReg(up(fetch1Action.OFFSET)(i)((cacheConfig.offsetWidth - 1) downto 2))
              up(fetch1Action.HIT)(i) := True
            }
          }.otherwise {
            when(up(fetch1Action.HIT)(0)) {
              resultReg(1) := (
                up(fetch1Action.CACHED)(1) ?
                  replaceWayDataReg(up(fetch1Action.OFFSET)(1)((cacheConfig.offsetWidth - 1) downto 2)) |
                  replaceWayDataReg(0)
              )
              up(fetch1Action.HIT)(1) := True
            }.otherwise {
              resultReg(0) := (
                up(fetch1Action.CACHED)(0) ?
                  replaceWayDataReg(up(fetch1Action.OFFSET)(0)((cacheConfig.offsetWidth - 1) downto 2)) |
                  replaceWayDataReg(0)
              )
              up(fetch1Action.HIT)(0) := True
            }
          }
          goto(stateBoot)
        }
      }
    }
    fetch2.haltWhen(!(instFetchFSM.isActive(instFetchFSM.stateBoot) && !(up.valid && !flush && (!up(fetch1Action.HIT)(0) || !up(fetch1Action.HIT)(1)))))
    fetchPorts.output.dout := fix ? resultReg | result
    fetchPorts.output.ready := up.isReady
    fetchPorts.output.valid := up.isMoving
  }
  fetch2Action.setName("fetch2").reflectNames()

  val fetchRamPortsInit = new Area {
    val targetWay = fetch2.up(fetch1Action.HIT)(0) ? fetch2.up(fetch1Action.TARGET_WAY)(1) | fetch2.up(fetch1Action.TARGET_WAY)(0)
    val cached = fetch2.up(fetch1Action.HIT)(0) ? fetch2.up(fetch1Action.CACHED)(1) | fetch2.up(fetch1Action.CACHED)(0)
    val index = fetch2.up(fetch1Action.HIT)(0) ? fetch2.up(fetch1Action.INDEX)(1) | fetch2.up(fetch1Action.INDEX)(0)
    val tag = fetch2.up(fetch1Action.HIT)(0) ? fetch2.up(fetch1Action.TAG)(1) | fetch2.up(fetch1Action.TAG)(0)
    for (i <- 0 until cacheConfig.ways) {
      val curRam = tagRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      val cachedFloodFill = fetch2Action.floodFillReady && cached && (targetWay === curWay)
      curRam.io.ena := True
      curRam.io.wea := cachedFloodFill ? B(1, 1 bits) | B(0, 1 bits)
      curRam.io.addra := cachedFloodFill ? index | fetch1Action.down(fetch1Action.INDEX)(0)
      curRam.io.dina := tag
      curRam.io.enb := True
      curRam.io.addrb := fetch1Action.down(fetch1Action.INDEX)(1)
    }

    for (i <- 0 until cacheConfig.ways) {
      val curRam = validRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      val cachedFloodFill = fetch2Action.floodFillReady && cached && (targetWay === curWay)
      val cacheOpInvalidate = cacheOpPort.valid
      curRam.io.ena := True
      curRam.io.wea := (cachedFloodFill | cacheOpInvalidate) ? B(1, 1 bits) | B(0, 1 bits)
      curRam.io.addra := cacheOpInvalidate ? cacheOpAction.index | (cachedFloodFill ? index | fetch1Action.down(fetch1Action.INDEX)(0))
      curRam.io.dina := U(1, 1 bits)
      curRam.io.enb := True
      curRam.io.addrb := fetch1Action.down(fetch1Action.INDEX)(1)
    }

    for (i <- 0 until cacheConfig.words) {
      for (j <- 0 until cacheConfig.ways) {
        val curRam = dataRams(i)(j)
        val curWay = U(j, log2Up(cacheConfig.ways) bits)
        val cachedFloodFill = fetch2Action.floodFillReady && cached && (targetWay === curWay)
        curRam.io.ena := True
        curRam.io.wea := cachedFloodFill ? B(4 bits, default -> True) | B(0, 4 bits)
        curRam.io.addra := cachedFloodFill ? index | fetch1Action.down(fetch1Action.INDEX)(0)
        curRam.io.dina := fetch2Action.replaceWayDataReg(i)
        curRam.io.enb := True
        curRam.io.web := B(0, 4 bits)
        curRam.io.addrb := fetch1Action.down(fetch1Action.INDEX)(1)
        curRam.io.dinb := U(0, 32 bits)
      }
    }
  }
  Builder(fetch1, fetch2, fetch1To2)
}

object ICacheVerilog extends App {
  Config.spinal.generateVerilog(new ICache(CacheConfig(2, 1 << 7, 64), 2, "sim"))
}