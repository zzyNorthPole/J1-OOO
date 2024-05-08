package j1ooo.cpu.decode

import j1ooo.cpu.execute.memory.MemoryRsInputBus
import j1ooo.cpu.execute.muldiv.MuldivRsInputBus
import j1ooo.cpu.execute.normal.NormalRsInputBus
import j1ooo.cpu.execute.{ArfQueryBus, RatBus, RobPushBus, RobQueryBus}
import j1ooo.cpu.fetch.{FetchException, FetchOutputBus}
import j1ooo.cpu.signal.{DispatchResult, Exception, InstType}
import j1ooo.gen.{BpuConfig, Config, RSConfig, RobConfig}
import spinal.core._
import spinal.lib.misc.pipeline.{Builder, CtrlLink, StageLink}
import spinal.lib.{MuxOH, PriorityMux, flowBitsPimped, master, memPimped, slave, traversableOnceBoolPimped}

class DecodePipeline(bpuConfig: BpuConfig, robConfig: RobConfig, rsConfig: RSConfig, decodePorts: Int) extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val inputPorts = slave(FetchOutputBus(bpuConfig, decodePorts))
    val ratRPorts = Vec(master(RatBus(robConfig, "read")), decodePorts * 2)
    val ratWPorts = Vec(master(RatBus(robConfig, "write")), decodePorts)
    val robPushPorts = master(RobPushBus(robConfig, bpuConfig, decodePorts))
    val robQueryPorts = master(RobQueryBus(robConfig, decodePorts))
    val arfQueryPorts = master(ArfQueryBus(decodePorts))
    val normalRsInputBus = Vec(master(NormalRsInputBus(robConfig, rsConfig)), decodePorts)
    val muldivRsInputBus = master(MuldivRsInputBus(robConfig))
    val memoryRsInputBus = master(MemoryRsInputBus(robConfig))
  }
  noIoPrefix()

  import io._

  val decode, renameAndDispatch = CtrlLink()
  val decodeToRenameAndDispatch = StageLink(decode.down, renameAndDispatch.up)
  val decodeAction = new decode.Area {
    decode.throwWhen(flush, usingReady = true)

    up.valid := inputPorts.valid

    inputPorts.ready := up.isReady

    val PC = insert(inputPorts.dout.pc)

    val PREDICT = insert(inputPorts.dout.predict)

    val decoder = Seq.fill(decodePorts) {
      new Decoder()
    }
    for (i <- 0 until decodePorts) {
      decoder(i).io.pc := inputPorts.dout.pc(i)
      decoder(i).io.inst := inputPorts.dout.inst(i).asBits
    }
    val R_PORTS = insert(Vec(decoder(0).io.rPorts, decoder(1).io.rPorts))
    val W_PORTS = insert(Vec(decoder(0).io.wPorts, decoder(1).io.wPorts))
    val IMM = insert(Vec(decoder(0).io.immediate, decoder(1).io.immediate))
    val CTRL = insert(Vec(decoder(0).io.ctrl, decoder(1).io.ctrl))

    val syscall = Vec(decoder(0).io.syscall, decoder(1).io.syscall)
    val break = Vec(decoder(0).io.break, decoder(1).io.break)
    val reserveInst = Vec(decoder(0).io.reserveInst, decoder(1).io.reserveInst)
    val copUnusable = Vec(decoder(0).io.copUnusable, decoder(1).io.copUnusable)
    val curException = Vec(FetchException(), decodePorts)
    for (i <- 0 until decodePorts) {
      curException(i).valid := inputPorts.dout.exDescription(i).valid || syscall(i) || break(i) || reserveInst(i) || copUnusable(i)
      curException(i).op := PriorityMux(
        Vec(
          inputPorts.dout.exDescription(i).valid,
          syscall(i),
          break(i),
          reserveInst(i),
          copUnusable(i)
        ),
        Vec(
          inputPorts.dout.exDescription(i).op,
          Exception.SYS(),
          Exception.TR(),
          Exception.RI(),
          Exception.CPU()
        )
      )
      curException(i).isTlbHit := inputPorts.dout.exDescription(i).isTlbHit
    }
    val EX_DESCRIPTION = insert(curException)
  }
  val renameAndDispatchAction = new renameAndDispatch.Area {
    renameAndDispatch.throwWhen(flush, usingReady = true)

    val renameAction = new Area {
      for (i <- 0 until decodePorts) {
        for (j <- 0 until 2) {
          ratRPorts(i * 2 + j).ratAddr := up(decodeAction.R_PORTS)(i).addr(j)
          /* ratRPorts(i * 2 + j).ratItem */
        }
      }

      val checkRaW = Vec(Bool(), 2)
      for (i <- 0 until 2) {
        checkRaW(i) := up(decodeAction.W_PORTS)(0).valid(0) && up(decodeAction.R_PORTS)(1).valid(i) && up(decodeAction.W_PORTS)(0).addr(0) === up(decodeAction.R_PORTS)(1).addr(i)
      }

      val checkWaW = Bool()
      checkWaW := up(decodeAction.W_PORTS)(0).valid(0) && up(decodeAction.W_PORTS)(1).valid(0) && up(decodeAction.W_PORTS)(0).addr(0) === up(decodeAction.W_PORTS)(1).addr(0)

      for (i <- 0 until decodePorts) {
        ratWPorts(i).valid.clear()
        ratWPorts(i).ratAddr := up(decodeAction.W_PORTS)(i).addr(0)
        ratWPorts(i).ratItem.inRob := up(decodeAction.W_PORTS)(i).valid(0)
        ratWPorts(i).ratItem.robAddr := robPushPorts.end @@ U(i, 1 bits)
      }
      when(checkWaW) {
        ratWPorts(0).valid.clear()
        ratWPorts(1).valid.setWhen(up.isFiring)
      }.otherwise {
        for (i <- 0 until decodePorts) {
          ratWPorts(i).valid.setWhen(up.isFiring && up(decodeAction.W_PORTS)(i).valid(0))
        }
      }
    }

    val dispatchAction = new Area {
      val normalDispatch = Vec(DispatchResult(), decodePorts)
      for (i <- 0 until decodePorts) normalDispatch(i) := DispatchResult.NORMAL0()
      when(normalRsInputBus(0).count > normalRsInputBus(1).count) {
        for (i <- 0 until decodePorts) normalDispatch(i) := DispatchResult.NORMAL0()
      }.elsewhen(normalRsInputBus(0).count < normalRsInputBus(1).count) {
        for (i <- 0 until decodePorts) normalDispatch(i) := DispatchResult.NORMAL1()
      }.otherwise {
        normalDispatch(0) := DispatchResult.NORMAL0()
        normalDispatch(1) := DispatchResult.NORMAL1()
      }
      val dispatchResult = Vec(DispatchResult(), decodePorts)
      for (i <- 0 until decodePorts) {
        dispatchResult(i) := MuxOH(
          Vec(
            !up(decodeAction.EX_DESCRIPTION)(i).valid && (up(decodeAction.CTRL)(i).alu.valid | up(decodeAction.CTRL)(i).ju.valid | up(decodeAction.CTRL)(i).tu.valid),
            !up(decodeAction.EX_DESCRIPTION)(i).valid && (up(decodeAction.CTRL)(i).mdu.valid | up(decodeAction.CTRL)(i).mfc0.valid | up(decodeAction.CTRL)(i).mtc0.valid),
            !up(decodeAction.EX_DESCRIPTION)(i).valid && up(decodeAction.CTRL)(i).lsu.valid,
            up(decodeAction.EX_DESCRIPTION)(i).valid || up(decodeAction.CTRL)(i).instType === InstType.TLB() || up(decodeAction.CTRL)(i).instType === InstType.ERET() || up(decodeAction.CTRL)(i).instType === InstType.OTHERS()
          ),
          Vec(
            normalDispatch(i),
            DispatchResult.MULDIV(),
            DispatchResult.MEMORY(),
            DispatchResult.NONE()
          )
        )
      }
      val dispatchReady = Vec(Bool(), decodePorts)
      for (i <- 0 until decodePorts) {
        dispatchReady(i) := (dispatchResult(0) === dispatchResult(1)).mux(
          False -> dispatchResult(i).mux(
            DispatchResult.NORMAL0 -> normalRsInputBus(0).ready(0),
            DispatchResult.NORMAL1 -> normalRsInputBus(1).ready(0),
            DispatchResult.MULDIV -> muldivRsInputBus.ready(0),
            DispatchResult.MEMORY -> memoryRsInputBus.ready(0),
            DispatchResult.NONE -> True
          ),
          True -> dispatchResult(i).mux(
            DispatchResult.NORMAL0 -> normalRsInputBus(0).ready(1),
            DispatchResult.NORMAL1 -> normalRsInputBus(1).ready(1),
            DispatchResult.MULDIV -> muldivRsInputBus.ready(1),
            DispatchResult.MEMORY -> memoryRsInputBus.ready(1),
            DispatchResult.NONE -> True
          )
        )
      }
      renameAndDispatch.haltWhen(!dispatchReady.andR)
      for (i <- 0 until decodePorts) {
        for (j <- 0 until 2) {
          normalRsInputBus(j).valid(i) := up.isFiring && (dispatchResult(i) === ((U(j, 1 bits) === U(1, 1 bits)) ? DispatchResult.NORMAL1() | DispatchResult.NORMAL0()))
          normalRsInputBus(j).payload(i).valid := True
          /* normalRsInputBus(j).payload(i).src */
          normalRsInputBus(j).payload(i).dest.valid := up(decodeAction.W_PORTS)(i).valid(0)
          normalRsInputBus(j).payload(i).dest.addr := robPushPorts.end @@ U(i, 1 bits)
          normalRsInputBus(j).payload(i).pc := up(decodeAction.PC)(i)
          normalRsInputBus(j).payload(i).offset := up(decodeAction.IMM)(i)
          normalRsInputBus(j).payload(i).predictPc := up(decodeAction.PREDICT).predictPc(i)
          normalRsInputBus(j).payload(i).ctrl.alu := up(decodeAction.CTRL)(i).alu
          normalRsInputBus(j).payload(i).ctrl.ju := up(decodeAction.CTRL)(i).ju
          normalRsInputBus(j).payload(i).ctrl.tu := up(decodeAction.CTRL)(i).tu
          normalRsInputBus(j).payload(i).ctrl.instType := up(decodeAction.CTRL)(i).instType
        }
        muldivRsInputBus.valid(i) := up.isFiring && (dispatchResult(i) === DispatchResult.MULDIV())
        muldivRsInputBus.payload(i).valid := True
        /* muldivRsInputBus.payload(i).src */
        muldivRsInputBus.payload(i).dest.valid := up(decodeAction.W_PORTS)(i).valid(0)
        muldivRsInputBus.payload(i).dest.addr := robPushPorts.end @@ U(i, 1 bits)
        muldivRsInputBus.payload(i).pc := up(decodeAction.PC)(i)
        muldivRsInputBus.payload(i).ctrl.mdu := up(decodeAction.CTRL)(i).mdu
        muldivRsInputBus.payload(i).ctrl.mtc0 := up(decodeAction.CTRL)(i).mtc0
        muldivRsInputBus.payload(i).ctrl.mfc0 := up(decodeAction.CTRL)(i).mfc0
        muldivRsInputBus.payload(i).ctrl.instType := up(decodeAction.CTRL)(i).instType
        memoryRsInputBus.valid(i) := up.isFiring && (dispatchResult(i) === DispatchResult.MEMORY())
        memoryRsInputBus.payload(i).valid := True
        /* memoryRsInputBus.payload(i).src */
        memoryRsInputBus.payload(i).dest.valid := up(decodeAction.W_PORTS)(i).valid(0)
        memoryRsInputBus.payload(i).dest.addr := robPushPorts.end @@ U(i, 1 bits)
        memoryRsInputBus.payload(i).pc := up(decodeAction.PC)(i)
        memoryRsInputBus.payload(i).offset := up(decodeAction.IMM)(i)
        memoryRsInputBus.payload(i).ctrl.lsu := up(decodeAction.CTRL)(i).lsu
        memoryRsInputBus.payload(i).ctrl.cacheOp := up(decodeAction.CTRL)(i).cacheOp
        memoryRsInputBus.payload(i).ctrl.instType := up(decodeAction.CTRL)(i).instType
      }

      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          robQueryPorts.addr(i)(j) := ratRPorts(i * decodePorts + j).ratItem.robAddr
        }
      }

      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          arfQueryPorts.addr(i)(j) := up(decodeAction.R_PORTS)(i).addr(j)
        }
      }

      // src
      for (i <- 0 until 2) {
        for (j <- 0 until 2) {
          normalRsInputBus(i).payload(0).src(j).valid := up(decodeAction.R_PORTS)(0).valid(j)
          normalRsInputBus(i).payload(0).src(j).addr := robQueryPorts.addr(0)(j)
          normalRsInputBus(i).payload(0).src(j).wake := (
            !up(decodeAction.R_PORTS)(0).valid(j) || (!ratRPorts(0 * decodePorts + j).ratItem.inRob || robQueryPorts.finish(0)(j))
          )
          normalRsInputBus(i).payload(0).src(j).available := normalRsInputBus(i).payload(0).src(j).wake
          normalRsInputBus(i).payload(0).src(j).data := MuxOH(
            Vec(
              !up(decodeAction.R_PORTS)(0).valid(j),
              up(decodeAction.R_PORTS)(0).valid(j) && !ratRPorts(0 * decodePorts + j).ratItem.inRob,
              up(decodeAction.R_PORTS)(0).valid(j) && ratRPorts(0 * decodePorts + j).ratItem.inRob && robQueryPorts.finish(0)(j),
              up(decodeAction.R_PORTS)(0).valid(j) && ratRPorts(0 * decodePorts + j).ratItem.inRob && !robQueryPorts.finish(0)(j),
            ),
            Vec(
              up(decodeAction.IMM)(0),
              arfQueryPorts.data(0)(j),
              robQueryPorts.data(0)(j),
              U(0, 32 bits)
            )
          )

          normalRsInputBus(i).payload(1).src(j).valid := up(decodeAction.R_PORTS)(1).valid(j)
          normalRsInputBus(i).payload(1).src(j).addr := renameAction.checkRaW(j) ? (robPushPorts.end @@ U(0, 1 bits)) | robQueryPorts.addr(1)(j)
          normalRsInputBus(i).payload(1).src(j).wake := (
            !up(decodeAction.R_PORTS)(1).valid(j) ||
              ((up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j)) &&
                (!ratRPorts(1 * decodePorts + j).ratItem.inRob || robQueryPorts.finish(1)(j)))
          )
          normalRsInputBus(i).payload(1).src(j).available := normalRsInputBus(i).payload(1).src(j).wake
          normalRsInputBus(i).payload(1).src(j).data := MuxOH(
            Vec(
              !up(decodeAction.R_PORTS)(1).valid(j),
              up(decodeAction.R_PORTS)(1).valid(j) && renameAction.checkRaW(j),
              up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && !ratRPorts(1 * decodePorts + j).ratItem.inRob,
              up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && ratRPorts(1 * decodePorts + j).ratItem.inRob && robQueryPorts.finish(1)(j),
              up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && ratRPorts(1 * decodePorts + j).ratItem.inRob && !robQueryPorts.finish(1)(j),
            ),
            Vec(
              up(decodeAction.IMM)(1),
              U(0, 32 bits),
              arfQueryPorts.data(1)(j),
              robQueryPorts.data(1)(j),
              U(0, 32 bits)
            )
          )
        }
      }
      for (j <- 0 until 2) {
        muldivRsInputBus.payload(0).src(j).valid := up(decodeAction.R_PORTS)(0).valid(j)
        muldivRsInputBus.payload(0).src(j).addr := robQueryPorts.addr(0)(j)
        muldivRsInputBus.payload(0).src(j).wake := (
          !up(decodeAction.R_PORTS)(0).valid(j) || (!ratRPorts(0 * decodePorts + j).ratItem.inRob || robQueryPorts.finish(0)(j))
          )
        muldivRsInputBus.payload(0).src(j).available := muldivRsInputBus.payload(0).src(j).wake
        muldivRsInputBus.payload(0).src(j).data := MuxOH(
          Vec(
            !up(decodeAction.R_PORTS)(0).valid(j),
            up(decodeAction.R_PORTS)(0).valid(j) && !ratRPorts(0 * decodePorts + j).ratItem.inRob,
            up(decodeAction.R_PORTS)(0).valid(j) && ratRPorts(0 * decodePorts + j).ratItem.inRob && robQueryPorts.finish(0)(j),
            up(decodeAction.R_PORTS)(0).valid(j) && ratRPorts(0 * decodePorts + j).ratItem.inRob && !robQueryPorts.finish(0)(j),
          ),
          Vec(
            up(decodeAction.IMM)(0),
            arfQueryPorts.data(0)(j),
            robQueryPorts.data(0)(j),
            U(0, 32 bits)
          )
        )

        muldivRsInputBus.payload(1).src(j).valid := up(decodeAction.R_PORTS)(1).valid(j)
        muldivRsInputBus.payload(1).src(j).addr := renameAction.checkRaW(j) ? (robPushPorts.end @@ U(0, 1 bits)) | robQueryPorts.addr(1)(j)
        muldivRsInputBus.payload(1).src(j).wake := (
          !up(decodeAction.R_PORTS)(1).valid(j) ||
            ((up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j)) &&
              (!ratRPorts(1 * decodePorts + j).ratItem.inRob || robQueryPorts.finish(1)(j)))
          )
        muldivRsInputBus.payload(1).src(j).available := muldivRsInputBus.payload(1).src(j).wake
        muldivRsInputBus.payload(1).src(j).data := MuxOH(
          Vec(
            !up(decodeAction.R_PORTS)(1).valid(j),
            up(decodeAction.R_PORTS)(1).valid(j) && renameAction.checkRaW(j),
            up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && !ratRPorts(1 * decodePorts + j).ratItem.inRob,
            up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && ratRPorts(1 * decodePorts + j).ratItem.inRob && robQueryPorts.finish(1)(j),
            up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && ratRPorts(1 * decodePorts + j).ratItem.inRob && !robQueryPorts.finish(1)(j),
          ),
          Vec(
            up(decodeAction.IMM)(1),
            U(0, 32 bits),
            arfQueryPorts.data(1)(j),
            robQueryPorts.data(1)(j),
            U(0, 32 bits)
          )
        )
      }
      for (j <- 0 until 2) {
        memoryRsInputBus.payload(0).src(j).valid := up(decodeAction.R_PORTS)(0).valid(j)
        memoryRsInputBus.payload(0).src(j).addr := robQueryPorts.addr(0)(j)
        memoryRsInputBus.payload(0).src(j).wake := (
          !up(decodeAction.R_PORTS)(0).valid(j) || (!ratRPorts(0 * decodePorts + j).ratItem.inRob || robQueryPorts.finish(0)(j))
          )
        memoryRsInputBus.payload(0).src(j).available := memoryRsInputBus.payload(0).src(j).wake
        memoryRsInputBus.payload(0).src(j).data := MuxOH(
          Vec(
            !up(decodeAction.R_PORTS)(0).valid(j),
            up(decodeAction.R_PORTS)(0).valid(j) && !ratRPorts(0 * decodePorts + j).ratItem.inRob,
            up(decodeAction.R_PORTS)(0).valid(j) && ratRPorts(0 * decodePorts + j).ratItem.inRob && robQueryPorts.finish(0)(j),
            up(decodeAction.R_PORTS)(0).valid(j) && ratRPorts(0 * decodePorts + j).ratItem.inRob && !robQueryPorts.finish(0)(j),
          ),
          Vec(
            up(decodeAction.IMM)(0),
            arfQueryPorts.data(0)(j),
            robQueryPorts.data(0)(j),
            U(0, 32 bits)
          )
        )

        memoryRsInputBus.payload(1).src(j).valid := up(decodeAction.R_PORTS)(1).valid(j)
        memoryRsInputBus.payload(1).src(j).addr := renameAction.checkRaW(j) ? (robPushPorts.end @@ U(0, 1 bits)) | robQueryPorts.addr(1)(j)
        memoryRsInputBus.payload(1).src(j).wake := (
          !up(decodeAction.R_PORTS)(1).valid(j) ||
            ((up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j)) &&
              (!ratRPorts(1 * decodePorts + j).ratItem.inRob || robQueryPorts.finish(1)(j)))
          )
        memoryRsInputBus.payload(1).src(j).available := memoryRsInputBus.payload(1).src(j).wake
        memoryRsInputBus.payload(1).src(j).data := MuxOH(
          Vec(
            !up(decodeAction.R_PORTS)(1).valid(j),
            up(decodeAction.R_PORTS)(1).valid(j) && renameAction.checkRaW(j),
            up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && !ratRPorts(1 * decodePorts + j).ratItem.inRob,
            up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && ratRPorts(1 * decodePorts + j).ratItem.inRob && robQueryPorts.finish(1)(j),
            up(decodeAction.R_PORTS)(1).valid(j) && !renameAction.checkRaW(j) && ratRPorts(1 * decodePorts + j).ratItem.inRob && !robQueryPorts.finish(1)(j),
          ),
          Vec(
            up(decodeAction.IMM)(1),
            U(0, 32 bits),
            arfQueryPorts.data(1)(j),
            robQueryPorts.data(1)(j),
            U(0, 32 bits)
          )
        )
      }

      robPushPorts.valid := up.isFiring
      renameAndDispatch.haltWhen(!robPushPorts.ready)

      robPushPorts.payload.pc := up(decodeAction.PC)
      robPushPorts.payload.instType := Vec(up(decodeAction.CTRL)(0).instType, up(decodeAction.CTRL)(1).instType)
      for (i <- 0 until decodePorts) {
        robPushPorts.payload.archDest(i).valid := up(decodeAction.W_PORTS)(i).valid(0)
        robPushPorts.payload.archDest(i).addr := up(decodeAction.W_PORTS)(i).addr(0)
      }
      robPushPorts.payload.predict := up(decodeAction.PREDICT)
      robPushPorts.payload.exDescription := up(decodeAction.EX_DESCRIPTION)
      val finish = Vec(Bool(), decodePorts)
      for (i <- 0 until decodePorts) {
        finish(i) := dispatchResult(i) === DispatchResult.NONE
      }
      robPushPorts.payload.finish := finish
      for (i <- 0 until decodePorts) {
        robPushPorts.payload.juOp(i) := up(decodeAction.CTRL)(i).ju.op
        robPushPorts.payload.rs(i) := up(decodeAction.R_PORTS)(i).addr(0)
        robPushPorts.payload.tlbOp(i) := up(decodeAction.CTRL)(i).tlbOp.op
        robPushPorts.payload.cp0(i) := up(decodeAction.CTRL)(i).mtc0
      }
      robPushPorts.payload.dispatchResult := dispatchResult
    }
  }
  Builder(decode, renameAndDispatch, decodeToRenameAndDispatch)
}

object DecodePipelineVerilog extends App {
  Config.spinal.generateVerilog(new DecodePipeline(
    bpuConfig = Config.bpuConfig, robConfig = Config.robConfig, rsConfig = Config.rsConfig, decodePorts = 2
  ))
}