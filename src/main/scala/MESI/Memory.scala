package MESI

import chisel3._
import chisel3.util._

class Memory extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val wen = Input(new Bool)
    val busIn = Input(new BusData)
    val busResp = Output(new BusData)

    // DEBUG info
    val memAddr = Output(UInt(addrBits.W))
  })
  //  val mem = RegInit(VecInit.fill(1<<addrBits)(0.U(cacheBlockBits.W)))
  val mem = SyncReadMem(1 << addrBits, UInt(cacheBlockBits.W))

  when(io.busIn.valid) {
    val addr = Cat(io.busIn.tag, io.busIn.index)
    io.memAddr := addr
    val wr = io.busIn.cacheBlock
    io.busResp := io.busIn
    io.busResp.pid := procNum.U(procNumBits.W)

    when(io.wen) {
      mem.write(addr, wr)
      printf("memory: writes %d into %d\n", wr, addr)
      io.busResp.cacheBlock := wr
      when(io.busIn.busTransaction === BusUpgrade) {
        io.busResp.busTransaction := BusUpgrade
      }.otherwise {
        io.busResp.busTransaction := Invalid
      }
    }.otherwise {
      io.busResp.cacheBlock := mem.read(addr)
      io.busResp.busTransaction := Fill
    }
  }.otherwise {
    io.memAddr := 0.U(addrBits.W)
    io.busResp := 0.U.asTypeOf(new BusData)
  }
}
