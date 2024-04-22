package MOESI_Diplomacy

import chisel3._
import chisel3.util._

class MOESIBundle(t: UInt, i: UInt, b: UInt) extends Bundle {
  val tag = t
  val index = i
  val cacheBlock = b

  def addr: UInt = {
    Cat(tag, index)
  }
}

object MOESIBundle {
  def apply(tag: UInt, index: UInt, block: UInt): MOESIBundle = {
    new MOESIBundle(tag, index, block)
  }

  def apply(p: MOESIPS): MOESIBundle = {
    apply(p.tagBits, p.indexBits, p.cacheBlockBits)
  }

  def apply(tagB: Int, indexB: Int, blockB: Int): MOESIBundle = {
    new MOESIBundle(
      UInt(tagB.W),
      UInt(indexB.W),
      UInt(blockB.W)
    )
  }
}

class BusData(ap: MOESIPS) extends Bundle with HasMOESIParameters {
  val pid = UInt(procNumBits.W)
  val busTransaction = UInt(busTransBits.W)
  val state = UInt(stateBits.W)
  val valid = Bool()

  val addrBundle = MOESIBundle(ap)
}

class BusBundle(mp: MOESIPS) extends Bundle {
  val busData = new BusData(mp)
  val flag = new Bool
}

