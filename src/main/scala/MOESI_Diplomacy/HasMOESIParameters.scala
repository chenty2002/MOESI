package MOESI_Diplomacy

import chisel3._
import chisel3.util._

trait HasMOESIParameters {
  val busTransBits: Int = 3
  def Repl: UInt = 6.U(busTransBits.W)
  def BusRd: UInt = 5.U(busTransBits.W)
  def BusRdX: UInt = 4.U(busTransBits.W)
  def BusUpgrade: UInt = 3.U(busTransBits.W)
  def Flush: UInt = 2.U(busTransBits.W)
  def Fill: UInt = 1.U(busTransBits.W)
  def Invalid: UInt = 0.U(busTransBits.W)

  val procOpBits: Int = 2
  def PrRd: UInt = 1.U(procOpBits.W)
  def PrWr: UInt = 2.U(procOpBits.W)

  val stateBits: Int = 3
  def Modified: UInt = 4.U(stateBits.W)
  def Owned: UInt = 3.U(stateBits.W)
  def Exclusive: UInt = 2.U(stateBits.W)
  def Shared: UInt = 1.U(stateBits.W)
  def Invalidated: UInt = 0.U(stateBits.W)

  val procNum: Int = 4
  val procNumBits = log2Up(procNum + 1)
  val cacheBlockNum: Int = 4
  val cacheBlockNumBits = log2Up(cacheBlockNum)
}
