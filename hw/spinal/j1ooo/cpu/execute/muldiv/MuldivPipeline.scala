package j1ooo.cpu.execute.muldiv

import j1ooo.cpu.commit.PipeToCp0BusMfc0
import j1ooo.cpu.execute.RobMuldivWriteBackBus
import j1ooo.cpu.execute.normal.BypassInputBus
import j1ooo.gen.{Config, RobConfig}
import spinal.core._
import spinal.lib.misc.pipeline.{Builder, CtrlLink, StageLink}
import spinal.lib.{MuxOH, master, slave}

class MuldivPipeline(robConfig: RobConfig) extends Component {
  val io = new Bundle {
    val flush = in Bool()

    val inputPorts = slave(MuldivRsOutputBus(robConfig))

    val bypassPorts = Vec(master(BypassInputBus(robConfig)), 2)

    val pipeToCp0BusMfc0 = master(PipeToCp0BusMfc0())

    val outputPorts = master(RobMuldivWriteBackBus(robConfig))

    val commitPorts = Vec(slave(HiLoWriteBus()), 2)
  }
  noIoPrefix()

  import io._

  val hilo = new HiLo()
  hilo.io.flush := flush

  val commitAction = new Area {
    for (i <- 0 until 2) {
      hilo.io.commitPorts(i) << commitPorts(i)
    }
  }
  val issue, execute, writeBack = CtrlLink()
  val issueToExecute = StageLink(issue.down, execute.up)
  val executeToWriteBack = StageLink(execute.down, writeBack.up)

  val issueAction = new issue.Area {
    issue.throwWhen(flush, usingReady = true)

    up.valid := inputPorts.valid

    inputPorts.ready := up.isReady

    val OP = insert(inputPorts.payload.ctrl.mdu.op)
    val DIN0 = insert(inputPorts.payload.src(0).data)
    val DIN1 = insert(inputPorts.payload.src(1).data)

    val DEST_VALID = insert(inputPorts.payload.dest.valid)
    val DEST_ADDR = insert(inputPorts.payload.dest.addr)

    val PC = insert(inputPorts.payload.pc)

    val CTRL_MDU = insert(inputPorts.payload.ctrl.mdu)
    val CTRL_MTC0 = insert(inputPorts.payload.ctrl.mtc0)
    val CTRL_MFC0 = insert(inputPorts.payload.ctrl.mfc0)
  }

  val executeAction = new execute.Area {
    execute.throwWhen(flush, usingReady = true)

    val mdu = new Mdu()
    mdu.io.flush := flush
    mdu.io.bus.valid := up.isValid && up(issueAction.CTRL_MDU).valid
    mdu.io.bus.op := up(issueAction.CTRL_MDU).op
    mdu.io.bus.din(0) := up(issueAction.DIN0)
    mdu.io.bus.din(1) := up(issueAction.DIN1)

    hilo.io.rPorts.op := up(issueAction.CTRL_MDU).op
    /* hilo.io.rPorts.dout */

    hilo.io.wPorts.valid := up.isFiring && up(issueAction.CTRL_MDU).valid
    hilo.io.wPorts.op := up(issueAction.CTRL_MDU).op
    for (i <- 0 until 2) {
      hilo.io.wPorts.din(i) := mdu.io.bus.dout(i)
    }

    val HILO_DIN = insert(mdu.io.bus.dout)

    pipeToCp0BusMfc0.addr := up(issueAction.CTRL_MFC0).addr
    pipeToCp0BusMfc0.select := up(issueAction.CTRL_MFC0).select
    /* pipeToCp0BusMfc0.dout */

    val DOUT = insert(MuxOH(
      Vec(
        up(issueAction.CTRL_MDU).valid,
        up(issueAction.CTRL_MFC0).valid
      ),
      Vec(
        hilo.io.rPorts.dout,
        pipeToCp0BusMfc0.dout
      )
    ))

    bypassPorts(0).valid := up.isFiring && up(issueAction.DEST_VALID)
    bypassPorts(0).addr := up(issueAction.DEST_ADDR)
    bypassPorts(0).data := down(DOUT)

    execute.haltWhen(up(issueAction.CTRL_MDU).valid && !mdu.io.bus.ready)
  }

  val writeBackAction = new writeBack.Area {
    writeBack.throwWhen(flush, usingReady = true)

    val pc = up(issueAction.PC)

    outputPorts.valid := up.isFiring
    outputPorts.addr := up(issueAction.DEST_ADDR)
    outputPorts.payload.op := up(issueAction.OP)
    outputPorts.payload.din1 := up(issueAction.DIN1)
    outputPorts.payload.hiloDin := up(executeAction.HILO_DIN)
    outputPorts.payload.dout := up(executeAction.DOUT)

    bypassPorts(1).valid := up.isFiring && up(issueAction.DEST_VALID)
    bypassPorts(1).addr := up(issueAction.DEST_ADDR)
    bypassPorts(1).data := up(executeAction.DOUT)
  }
  Builder(issue, execute, writeBack, issueToExecute, executeToWriteBack)
}

object MuldivPipelineVerilog extends App {
  Config.spinal.generateVerilog(new MuldivPipeline(Config.robConfig))
}