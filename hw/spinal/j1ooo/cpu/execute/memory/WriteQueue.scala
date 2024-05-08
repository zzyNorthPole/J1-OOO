package j1ooo.cpu.execute.memory

import j1ooo.cpu.signal.LsuOp
import j1ooo.cpu.util.BusRename
import j1ooo.gen.{Config, WriteQueueConfig}
import spinal.core._
import spinal.lib.{IMasterSlave, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4WriteOnly}
import spinal.lib.fsm.{State, StateMachine}

case class WriteQueueItem() extends Bundle {
  val op = LsuOp()
  val addr = UInt(32 bits)
  val data = Bits(32 bits)
}

case class WriteQueueBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val payload = WriteQueueItem()

  override def asMaster(): Unit = {
    out(valid, payload)
  }

  def << (that: WriteQueueBus): Unit = {
    this.valid := that.valid
    this.payload := that.payload
  }
}

class WriteQueue(writeQueueConfig: WriteQueueConfig, axi4Config: Axi4Config) extends Component {
  val io = new Bundle {
    val empty = out Bool()
    val full = out Bool()

    val input = slave(WriteQueueBus())

    val udbusWrite = master(Axi4WriteOnly(axi4Config)).setIdle()
  }
  noIoPrefix()
  BusRename.setName(io.udbusWrite)

  import io._

  val front = RegInit(U(0, log2Up(writeQueueConfig.lines) bits))
  val end = RegInit(U(0, log2Up(writeQueueConfig.lines) bits))

  // val writeQueue = Vec(Reg(new WriteQueueItem), writeQueueConfig.lines)
  val writeQueue = Mem(WriteQueueItem(), writeQueueConfig.lines)

  val udbusWriteInit = new Area {

    import udbusWrite._

    val frontItem = writeQueue.readAsync(address = front)

    aw.addr := frontItem.addr
    aw.id := U(1, 4 bits);
    aw.len := U(0, 8 bits)
    aw.size := frontItem.op.mux(
      LsuOp.B -> U(0, 3 bits),
      LsuOp.H -> U(1, 3 bits),
      default -> U(2, 3 bits)
    )
    aw.burst := B(1, 2 bits) // INCR
    // aw.lock := B(0, 2 bits)
    aw.cache := B(0, 4 bits)
    aw.prot := B(0, 3 bits)

    w.data := frontItem.data
    w.strb := frontItem.op.mux(
      LsuOp.B -> frontItem.addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, B(4 bits, i -> True, default -> False))
      ),
      LsuOp.H -> (frontItem.addr(1) ? B"1100" | B"0011"),
      LsuOp.W -> B"1111",
      LsuOp.WL -> frontItem.addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, B(4 bits, (i downto 0) -> True, default -> False))
      ),
      LsuOp.WR -> frontItem.addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, B(4 bits, (3 downto i) -> True, default -> False))
      ),
      default -> B"0000"
    )
    w.last := True
  }

  empty := front === end
  full := (end + 1) === front

  when(input.valid) {
    end := end + 1
    writeQueue.write(
      address = end,
      data = input.payload
    )
  }

  val dataUncachedWriteFSM = new StateMachine {

    import udbusWrite._

    setEntry(stateBoot)
    disableAutoStart()

    val writeAw = new State()
    val writeW = new State()
    val writeB = new State()

    stateBoot.whenIsActive {
      when(!empty) {
        goto(writeAw)
      }
    }

    writeAw.whenIsActive {
      aw.valid := True
      when(aw.ready) {
        goto(writeW)
      }
    }

    writeW.whenIsActive {
      w.valid := True
      when(w.ready) {
        goto(writeB)
      }
    }

    writeB.whenIsActive {
      b.ready := True
      when(b.valid) {
        front := front + 1
        goto(stateBoot)
      }
    }
  }
}

object WriteQueueVerilog extends App {
  Config.spinal.generateVerilog(new WriteQueue(writeQueueConfig = Config.writeQueueConfig, axi4Config = Config.axi4Config))
}