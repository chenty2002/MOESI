package MOESI

import chisel3._

class BusData extends Bundle with HasMESIParameters {
  val pid = UInt(procNumBits.W)
  val busTransaction = UInt(busTransBits.W)
  val tag = UInt(tagBits.W)
  val index = UInt(indexBits.W)
  val cacheBlock = UInt(cacheBlockBits.W)
  val state = UInt(stateBits.W)
  val valid = Bool()

  def addr: UInt = {
    getAddr(index, tag)
  }
}
