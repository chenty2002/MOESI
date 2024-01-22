package MESI

import chisel3._
import chisel3.util._

trait HasMESIParameters {
  val busOpBits: Int = 3
  def BusRd: UInt = 5.U(busOpBits.W)
  def BusRdX: UInt = 4.U(busOpBits.W)
  def BusUpgrade: UInt = 3.U(busOpBits.W)
  def Flush: UInt = 2.U(busOpBits.W)
  def Fill: UInt = 1.U(busOpBits.W)
  def Invalid: UInt = 0.U(busOpBits.W)

  val coreOpBits: Int = 2
  def PrRd: UInt = 1.U(coreOpBits.W)
  def PrWr: UInt = 2.U(coreOpBits.W)

  val stateBits: Int = 2
  def Modified: UInt = 3.U(stateBits.W)
  def Exclusive: UInt = 2.U(stateBits.W)
  def Shared: UInt = 1.U(stateBits.W)
  def Invalidated: UInt = 0.U(stateBits.W)

  val coreNum: Int = 2
  val coreNumBits = log2Up(coreNum)
  val pidBits: Int = 2
  val tagBits: Int = 2
  val indexBits: Int = 2
  val cacheBlockNum: Int = 4
  val cacheBlockNumBits = log2Up(cacheBlockNum)
  val cacheBlockBits: Int = 8

  val addrBits: Int = tagBits + indexBits

  def parseAddr(addr: UInt): (UInt, UInt) = {
    val indexStart = 0
    val indexEnd = indexStart + indexBits - 1
    val index = addr(indexEnd, indexStart)
    val tagStart = indexEnd
    val tagEnd = tagStart + tagBits - 1
    val tag = addr(tagEnd, tagStart)
    (index, tag)
  }

  def getAddr(index: UInt, tag: UInt): UInt = {
    Cat(index, tag)
  }
}
