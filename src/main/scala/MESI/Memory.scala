package MESI

import chisel3._
import chisel3.util._

class Memory extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val addr = Input(UInt(addrBits.W))
    val rd = Output(Vec(1<<addrBits, UInt(cacheBlockBits.W)))
    val wr = Input(UInt(cacheBlockBits.W))
    val wen = Input(new Bool)
  })

  val mem = RegInit(VecInit.fill(1<<addrBits)(0.U(cacheBlockBits.W)))

  io.rd := mem
  when(io.wen) {
    mem(io.addr) := io.wr
  }
}
