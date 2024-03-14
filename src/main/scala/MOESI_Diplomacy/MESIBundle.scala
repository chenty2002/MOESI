package MOESI_Diplomacy

import chisel3._
import chisel3.util._

class MESIBundle(t: UInt, i: UInt, b: UInt) extends Bundle {
  val tag = t
  val index = i
  val cacheBlock = b

  def addr: UInt = {
    Cat(tag, index)
  }
}

object MESIBundle {
  def apply(tag: UInt, index: UInt, block: UInt): MESIBundle = {
    new MESIBundle(tag, index, block)
  }

  def apply(p: MESIPS): MESIBundle = {
    apply(p.tagBits, p.indexBits, p.cacheBlockBits)
  }

  def apply(tagB: Int, indexB: Int, blockB: Int): MESIBundle = {
    new MESIBundle(
      UInt(tagB.W),
      UInt(indexB.W),
      UInt(blockB.W)
    )
  }
}

class BusData(ap: MESIPS) extends Bundle with HasMOESIParameters {
  val pid = UInt(procNumBits.W)
  val busTransaction = UInt(busTransBits.W)
  val state = UInt(stateBits.W)
  val valid = Bool()

  val addrBundle = MESIBundle(ap)
}

class BusBundle(mp: MESIPS) extends Bundle {
  val busData = new BusData(mp)
  val flag = new Bool
}

