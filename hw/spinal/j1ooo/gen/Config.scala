package j1ooo.gen

import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4Config

case class BtbConfig(
                    lines: Int,
                    ways: Int
                    ) {
  def indexWidth = log2Up(lines)
  def offsetWidth = log2Up(ways)
  def tagWidth = 32 - indexWidth - offsetWidth - 2
}

case class BhtConfig(
                    lines: Int,
                    ways: Int,
                    histories: Int
                    ) {
  def indexWidth = log2Up(lines)
  def offsetWidth = log2Up(ways)
  assert(indexWidth + histories <= 12)
}

case class PhtConfig(
                    lines: Int,
                    fetchPorts: Int
                    ) {
  def indexWidth = log2Up(lines)
  assert(lines <= (1 << 12))
}

case class RasConfig(
                    lines: Int,
                    width: Int
                    ) {
  // temp
  assert(lines == 4)
}

case class BpuConfig(
                    btbConfig: BtbConfig,
                    bhtConfig: BhtConfig,
                    phtConfig: PhtConfig,
                    rasConfig: RasConfig
                    )

case class CacheConfig(
                      ways: Int,
                      lines: Int,
                      blockSize: Int
                      ) {
  def indexWidth = log2Up(lines)
  def offsetWidth = log2Up(blockSize)
  def tagWidth = 32 - indexWidth - offsetWidth
  def words = blockSize / 4
}

case class FetchFIFOConfig(
                          lines: Int
                          ) {
  def indexWidth = log2Up(lines)
}

case class RobConfig(
                    lines: Int
                    )

case class TlbConfig(
                    ports: Int,
                    lines: Int
                    )

case class MSHRConfig(
                     lines: Int
                     )

case class LSTConfig(
                    lines: Int
                    )

case class WriteQueueConfig(
                           lines: Int
                           )

case class WriteBufferConfig(
                            lines: Int
                            )

case class StoreBufferConfig(
                            lines: Int
                            )

case class RSConfig(
                   normal: Int,
                   muldiv: Int,
                   memory: Int
                   )

object Config {
  def bpuConfig = BpuConfig(
    btbConfig = BtbConfig(
      lines = 1 << 7,
      ways = 2
    ),
    bhtConfig = BhtConfig(
      lines = 1 << 7,
      ways = 2,
      histories = 5
    ),
    phtConfig = PhtConfig(
      lines = 1 << 10,
      fetchPorts = 2
    ),
    rasConfig = RasConfig(
      lines = 1 << 2,
      width = 16
    )
  )

  def fetchFIFOConfig = FetchFIFOConfig(
    lines = 8
  )

  def iCacheConfig = CacheConfig(
    ways = 2,
    lines = 1 << 6,
    blockSize = 64
  )

  def dCacheConfig = CacheConfig(
    ways = 4,
    lines = 1 << 6,
    blockSize = 64
  )

  def storeBufferConfig = StoreBufferConfig(
    lines = 4
  )

  def writeBufferConfig = WriteBufferConfig(
    lines = 4
  )

  def tlbConfig = TlbConfig(
    ports = 3,
    lines = 8
  )

  def robConfig = RobConfig(
    lines = 64
  )

  def rsConfig = RSConfig(
    normal = 5, muldiv = 4, memory = 4
  )

  def mshrConfig = MSHRConfig(
    lines = 2
  )

  def lstConfig = LSTConfig(
    lines = 1
  )

  def writeQueueConfig = WriteQueueConfig(
    lines = 16
  )
  def axi4Config = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 4,
    useRegion = false,
    useLock = false,
    useQos = false
  )

  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      clockEdge = RISING, resetKind = SYNC, resetActiveLevel = HIGH
    )
  )
}
