package j1ooo.cpu.util

import spinal.core.Bundle
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnly, Axi4WriteOnly}

object BusRename {
  def setName(bus: Bundle) = {
    bus.flattenForeach {
      signal => {
        val name = signal.getName()
        val list = name.split("_")
        signal.setName(list(0) + "_" + list(1) + list(list.size - 1))
      }
    }
    bus
  }
}
