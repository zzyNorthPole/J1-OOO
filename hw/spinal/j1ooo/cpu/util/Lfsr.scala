package j1ooo.cpu.util

import spinal.core._

class Lfsr(width: Int) extends Component {
  val io = new Bundle {
    val en = in Bool()
    val seed = in UInt (width bits)
    val dout = out UInt (width bits)
  }

  noIoPrefix()

  val tapsSet = Seq(
    U"32'h1", U"32'h1", U"32'h3", U"32'h5",
    U"32'h9", U"32'h12", U"32'h21", U"32'h41",
    U"32'h8e", U"32'h108", U"32'h204", U"32'h402",
    U"32'h829", U"32'h100d", U"32'h2015", U"32'h4001",
    U"32'h8016", U"32'h10004", U"32'h20040", U"32'h40013",
    U"32'h80004", U"32'h100002", U"32'h200001", U"32'h400010",
    U"32'h80000d", U"32'h1000004", U"32'h2000023", U"32'h4000013",
    U"32'h8000004", U"32'h10000002", U"32'h20000029", U"32'h40000004",
    U"32'h80000062"
  )
  val taps = tapsSet(32 - width)

  import io._

  val lfsr = RegInit(seed)
  when(en) {
    lfsr := lfsr((width - 2) downto 0) @@ (lfsr & taps(width - 1 downto 0)).xorR
  }
  dout := lfsr
}
