package j1ooo.cpu.util

import j1ooo.gen.Config
import spinal.core._
import spinal.core.sim.SimMemPimper
import spinal.lib.misc.pipeline.{Builder, CtrlLink, Node, Payload, StageLink}

//class Cpu extends Component {
////  val fetch, decode, execute = CtrlLink()
////  val f2d = StageLink(fetch.down, decode.up).withoutCollapse()
////  val d2e = StageLink(decode.down, execute.up).withoutCollapse()
////
////  val PC = Payload(UInt(8 bits))
////  val INSTRUCTION = Payload(Bits(16 bits))
////
////  val led = out(Reg(Bits(8 bits))) init(0)
////
////  val fetcher = new fetch.Area{
////    val microop = INSTRUCTION(7 downto 4)
////    val MICRO_IS_DELAY = insert(microop === 0x1)
////
////    val pcReg = Reg(PC) init (0)
////    up(PC) := pcReg
////    up.valid := True
////    when(up.isFiring) {
////      pcReg := PC + 1
////    }
////
////    val mem = Mem.fill(256)(INSTRUCTION).simPublic
////    INSTRUCTION := mem.readAsync(PC)
////  }
////
////  val decoder = new decode.Area{
////    val opcode = INSTRUCTION(7 downto 0)
////    val IS_ADD   = insert(opcode === 0x1)
////    val IS_JUMP  = insert(opcode === 0x2)
////    val IS_LED   = insert(opcode === 0x3)
////    val IS_DELAY = insert(opcode === 0x4)
////
////    val count = RegInit(False)
////    when(fetcher.MICRO_IS_DELAY) {
////      count := ~count
////      when(!count) {
////        decode.haltIt()
////      }
////    }
////  }
////
////
////  val alu = new execute.Area{
////    val regfile = Reg(UInt(8 bits)) init(0)
////
////    val flush = False
////    for (stage <- List(fetch, decode)) {
////      stage.throwWhen(flush, usingReady = true)
////    }
////
////    val delayCounter = Reg(UInt(8 bits)) init (0)
////
////    when(isValid) {
////      when(decoder.IS_ADD) {
////        up(decoder.IS_ADD) := False
////        regfile := regfile + U(INSTRUCTION(15 downto 8))
////      }
////      when(decoder.IS_JUMP) {
////        flush := True
////        fetcher.pcReg := U(INSTRUCTION(15 downto 8))
////      }
////      when(decoder.IS_LED) {
////        led := B(regfile)
////      }
////      when(decoder.IS_DELAY) {
////        delayCounter := delayCounter + 1
////        when(delayCounter === U(INSTRUCTION(15 downto 8))) {
////          delayCounter := 0
////        } otherwise {
////          execute.haltIt()
////        }
////      }
////    }
////  }
////
////  Builder(fetch, decode, execute, f2d, d2e)
//}

//object TestVerilog extends App {
//  Config.spinal.generateVerilog(new Cpu())
//}