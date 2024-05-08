package j1ooo.cpu.execute.normal

import j1ooo.cpu.execute.RobNormalWriteBackBus
import j1ooo.cpu.signal.Exception
import j1ooo.gen.{Config, RobConfig}
import spinal.core._
import spinal.lib.misc.pipeline.{Builder, CtrlLink, StageLink}
import spinal.lib.{MuxOH, master, slave}

class NormalPipeline(robConfig: RobConfig) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val inputPorts = slave(NormalRsOutputBus(robConfig))

    val bypassPorts = Vec(master(BypassInputBus(robConfig)), 2)

    val outputPorts = master(RobNormalWriteBackBus(robConfig))
  }
  noIoPrefix()

  import io._

  val issue, execute, writeBack = CtrlLink()
  val issueToExecute = StageLink(issue.down, execute.up)
  val executeToWriteBack = StageLink(execute.down, writeBack.up)

  val issueAction = new issue.Area {
    issue.throwWhen(flush, usingReady = true)

    up.valid := inputPorts.valid

    inputPorts.ready := True

    val DIN0 = insert(inputPorts.payload.src(0).data)
    val DIN1 = insert(inputPorts.payload.src(1).data)

    val DEST_VALID = insert(inputPorts.payload.dest.valid)
    val DEST_ADDR = insert(inputPorts.payload.dest.addr)

    val PC = insert(inputPorts.payload.pc)
    val OFFSET = insert(inputPorts.payload.offset)

    val PREDICT_PC = insert(inputPorts.payload.predictPc)

    val CTRL_ALU = insert(inputPorts.payload.ctrl.alu)
    val CTRL_JU = insert(inputPorts.payload.ctrl.ju)
    val CTRL_TU = insert(inputPorts.payload.ctrl.tu)
    // val INST_TYPE = insert(inputPorts.payload.ctrl.instType)
  }

  val executeAction = new execute.Area {
    execute.throwWhen(flush, usingReady = true)

    val alu = new Alu()
    alu.io.bus.valid := up.isValid && up(issueAction.CTRL_ALU).valid
    alu.io.bus.op := up(issueAction.CTRL_ALU).op
    alu.io.bus.din(0) := up(issueAction.DIN0)
    alu.io.bus.din(1) := up(issueAction.DIN1)
    /* alu.io.bus.dout */

    val MOV_FAIL = insert(alu.io.movFail)
    val overflow = alu.io.overflow

    val ju = new Ju()
    ju.io.bus.valid := up.isValid && up(issueAction.CTRL_JU).valid
    ju.io.bus.op := up(issueAction.CTRL_JU).op
    ju.io.bus.din(0) := up(issueAction.DIN0).asSInt
    ju.io.bus.din(1) := up(issueAction.DIN1).asSInt
    ju.io.bus.pc := up(issueAction.PC)
    ju.io.bus.offset := up(issueAction.OFFSET)
    ju.io.bus.predictPc := up(issueAction.PREDICT_PC)
    /* ju.io.bus.dout */

    val PREDICT_FAIL = insert(ju.io.checkResult.fail)
    val NEXT_PC = insert(ju.io.checkResult.pc)

    val BPU_NEXT_HISTORY = insert(ju.io.bpuResult.fail)
    val BPU_TARGET_PC = insert(ju.io.bpuResult.pc)

    val tu = new Tu()
    tu.io.bus.valid := up.isValid && up(issueAction.CTRL_TU).valid
    tu.io.bus.op := up(issueAction.CTRL_TU).op
    tu.io.bus.din(0) := up(issueAction.DIN0)
    tu.io.bus.din(1) := up(issueAction.DIN1)

    val trap = tu.io.trap

    val DOUT = insert(MuxOH(
      Vec(
        up(issueAction.CTRL_ALU).valid,
        up(issueAction.CTRL_JU).valid,
        up(issueAction.CTRL_TU).valid
      ),
      Vec(
        alu.io.bus.dout,
        ju.io.bus.dout,
        U(0, 32 bits)
      )
    ))

    val EX_VALID = insert(up.isValid && (overflow || trap))
    val EX_OP = insert(MuxOH(
      Vec(
        overflow,
        trap
      ),
      Vec(
        Exception.OV(),
        Exception.TR()
      )
    ))

    bypassPorts(0).valid := up.isFiring && up(issueAction.DEST_VALID) && !down(EX_VALID)
    bypassPorts(0).addr := up(issueAction.DEST_ADDR)
    bypassPorts(0).data := down(DOUT)
  }

  val writeBackAction = new writeBack.Area {
    writeBack.throwWhen(flush, usingReady = true)

    outputPorts.valid := up.isFiring
    outputPorts.addr := up(issueAction.DEST_ADDR)
    outputPorts.payload.dout := up(executeAction.DOUT)
    outputPorts.payload.movFail := up(executeAction.MOV_FAIL)
    outputPorts.payload.predictRes.fail := up(executeAction.PREDICT_FAIL)
    outputPorts.payload.predictRes.nextPc := up(executeAction.NEXT_PC)

    outputPorts.payload.bpu.nextHistory := up(executeAction.BPU_NEXT_HISTORY)
    outputPorts.payload.bpu.targetPc := up(executeAction.BPU_TARGET_PC)
    outputPorts.payload.exception.valid := up(executeAction.EX_VALID)
    outputPorts.payload.exception.op := up(executeAction.EX_OP)

    bypassPorts(1).valid := up.isFiring && up(issueAction.DEST_VALID) && !up(executeAction.EX_VALID)
    bypassPorts(1).addr := up(issueAction.DEST_ADDR)
    bypassPorts(1).data := up(executeAction.DOUT)
  }
  Builder(issue, execute, writeBack, issueToExecute, executeToWriteBack)
}

object NormalPipelineVerilog extends App {
  Config.spinal.generateVerilog(new NormalPipeline(Config.robConfig))
}