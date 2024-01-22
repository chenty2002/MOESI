package MESI

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class MESITop() extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val coreOp = Input(Vec(coreNum, UInt(coreOpBits.W)))
    val addr = Input(Vec(coreNum, UInt(addrBits.W)))
    val cacheInput = Input(Vec(coreNum, UInt(cacheBlockBits.W)))
    val cacheOutput = Output(Vec(coreNum, UInt(cacheBlockBits.W)))
  })

  val l1s = (0 until coreNum).map(i => Module(new L1Cache(hostPid = i.U(coreNumBits.W))))
  val mem = Module(new Memory)

  val memWen = RegInit(false.B)
  val memAddr = RegInit(0.U(addrBits.W))
  val memWr = RegInit(0.U(addrBits.W))

  mem.io.wen := memWen
  mem.io.addr := memAddr
  mem.io.wr := memWr

  l1s.foreach(_.io.mem := mem.io.rd)
  for(i <- l1s.indices) {
    when(l1s(i).io.memWen) {
      memWen := true.B
      memAddr := l1s(i).io.memAddr
      memWr := l1s(i).io.memWr
    }
  }

  l1s(0).io.coreOp := io.coreOp(0)
  l1s(0).io.prAddr := io.addr(0)
  l1s(0).io.cacheInput := io.cacheInput(0)
  io.cacheOutput(0) := l1s(0).io.cacheOutput
  l1s(0).io.busIn := l1s(1).io.busOut
  l1s(0).io.busValid := l1s(1).io.validateBus

  l1s(1).io.coreOp := io.coreOp(1)
  l1s(1).io.prAddr := io.addr(1)
  l1s(1).io.cacheInput := io.cacheInput(1)
  io.cacheOutput(1) := l1s(1).io.cacheOutput
  l1s(1).io.busIn := l1s(0).io.busOut
  l1s(1).io.busValid := l1s(0).io.validateBus
}

object MESITop extends App {
  (new ChiselStage).emitVerilog(new MESITop(), Array("--target-dir", "Verilog/MESI"))
}