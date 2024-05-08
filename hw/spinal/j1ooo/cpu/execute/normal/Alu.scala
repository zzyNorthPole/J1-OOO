package j1ooo.cpu.execute.normal

import j1ooo.cpu.signal.AluOp
import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class AluBus() extends Bundle with IMasterSlave {
  val valid = Bool()
  val op = AluOp()
  val din = Vec(UInt(32 bits), 2)
  val dout = UInt()

  override def asMaster(): Unit = {
    out(valid)
    out(op)
    for (i <- 0 until 2) out(din(i))
    in(dout)
  }
}

class Alu extends Component {
  val io = new Bundle {
    val bus = slave(AluBus())
    val movFail = out Bool()
    val overflow = out Bool()
  }
  noIoPrefix()

  def cl(flag: Bool, din: UInt): UInt = {
    val rev = UInt(32 bits)
    rev := flag ? ~din.reversed | din.reversed
    val tmp = Vec(Vec(UInt(16 bits), 2), 5)
    val check = Vec(Bool(), 5)
    for (i <- 4 downto 0) {
      for (j <- 0 until 2) {
        if (i == 4) {
          tmp(i)(j) := rev(((j + 1) * (1 << i) - 1) downto (j * (1 << i)))
        }
        else {
          tmp(i)(j) := U(0, (16 - (1 << i)) bits) @@ tmp(i + 1)(check(i + 1).asUInt)(((j + 1) * (1 << i) - 1) downto (j * (1 << i)))
        }
      }
    }
    for (i <- 4 downto 0) {
      check(i) := (tmp(i)(0) === U(0, 16 bits))
    }
    val curDout = UInt(5 bits)
    for (i <- 4 downto 0) curDout(i) := check(i)
    val dout = UInt(32 bits)
    dout := (curDout.resize(6 bits) + ((tmp(0)(0) === U(0, 16 bits)) & (tmp(0)(1) === U(0, 16 bits))).asUInt.resize(6 bits)).resize(32 bits)
    dout
  }

  import io._

  import AluOp._

  val shamt = bus.din(0)(4 downto 0)
  bus.dout := bus.op.mux(
    ADD -> (bus.din(0).asSInt + bus.din(1).asSInt).asUInt,
    ADDU -> (bus.din(0) + bus.din(1)),
    SUB -> (bus.din(0).asSInt - bus.din(1).asSInt).asUInt,
    SUBU -> (bus.din(0) - bus.din(1)),
    AND -> (bus.din(0) & bus.din(1)),
    OR -> (bus.din(0) | bus.din(1)),
    XOR -> (bus.din(0) ^ bus.din(1)),
    NOR -> (~(bus.din(0) | bus.din(1))),
    SLT -> (S(0, 31 bits) @@ (bus.din(0).asSInt < bus.din(1).asSInt)).asUInt,
    SLTU -> (U(0, 31 bits) @@ (bus.din(0) < bus.din(1))),
    SLL -> (bus.din(1) |<< shamt),
    SRL -> (bus.din(1) |>> shamt),
    SRA -> (bus.din(1).asSInt >> shamt).asUInt,
    LUI -> (bus.din(1)(15 downto 0) @@ U(0, 16 bits)),
    MOVN -> bus.din(0),
    MOVZ -> bus.din(0),
    CLO -> cl(True, bus.din(0)),
    CLZ -> cl(False, bus.din(0))
  )

  movFail := bus.valid && bus.op.mux(
    MOVN -> (bus.din(1) === U(0, 32 bits)),
    MOVZ -> (bus.din(1) =/= U(0, 32 bits)),
    default -> False
  )

  overflow := bus.valid && bus.op.mux(
    ADD -> (bus.din(0)(31) === bus.din(1)(31) && bus.din(0)(31) =/= bus.dout(31)),
    SUB -> (bus.din(0)(31) =/= bus.din(1)(31) && bus.din(0)(31) =/= bus.dout(31)),
    default -> False
  )

}
