package MESI

import chisel3._
import chisel3.util._

class BusData extends Bundle with HasMESIParameters {
  val pid = UInt(pidBits.W)
  val busTransaction = UInt(busOpBits.W)
  val tag = UInt(tagBits.W)
  val index = UInt(indexBits.W)
  val cacheBlock = UInt(cacheBlockBits.W)
  val state = UInt(stateBits.W)
  val valid = Bool()

  def addr: UInt = {
    getAddr(index, tag)
  }
}
