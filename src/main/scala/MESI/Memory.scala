package MESI

import chisel3._
import chisel3.util._

class Memory extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val wen = Input(new Bool)
    val busIn = Input(new BusData)
    val busResp = Output(new BusData)
  })
//  val mem = RegInit(VecInit.fill(1<<addrBits)(0.U(cacheBlockBits.W)))
  val mem = SyncReadMem(1<<addrBits, UInt(cacheBlockBits.W))

  when(io.busIn.valid) {
    val addr = Cat(io.busIn.tag, io.busIn.index)
    val wr = io.busIn.cacheBlock
    io.busResp := io.busIn
    io.busResp.busTransaction := Fill

    when(io.wen) {
      mem.write(addr, wr)
      io.busResp.cacheBlock := wr
    }.otherwise {
      io.busResp.cacheBlock := mem.read(addr)
    }
  }.otherwise {
    io.busResp := 0.U.asTypeOf(new BusData)
  }
}
