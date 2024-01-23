package MESI

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class MESITop() extends Module with HasMESIParameters {
  /*
  proc0   proc1
    |       |
 l1cache l1cache
     \      /
      memory
   */

  val io = IO(new Bundle() {
    val procOp = Input(Vec(procNum, UInt(procOpBits.W)))
    val addr = Input(Vec(procNum, UInt(addrBits.W)))
    val cacheInput = Input(Vec(procNum, UInt(cacheBlockBits.W)))
    val cacheOutput = Output(Vec(procNum, UInt(cacheBlockBits.W)))
    val memory = Output(Vec(1<<addrBits, UInt(cacheBlockBits.W)))
  })

  val l1s = (0 until procNum).map(i => Module(new L1Cache(hostPid = i.U(procNumBits.W))))
  val mem = Module(new Memory)

  val memWen = RegInit(false.B)
  val memAddr = RegInit(0.U(addrBits.W))
  val memWr = RegInit(0.U(addrBits.W))

  mem.io.wen := memWen
  mem.io.addr := memAddr
  mem.io.wr := memWr
  io.memory := mem.io.rd

  l1s.foreach(_.io.mem := mem.io.rd)
  for(i <- l1s.indices) {
    when(l1s(i).io.memWen) {
      memWen := true.B
      memAddr := l1s(i).io.memAddr
      memWr := l1s(i).io.memWr
    }
  }

  l1s(0).io.procOp := io.procOp(0)
  l1s(0).io.prAddr := io.addr(0)
  l1s(0).io.cacheInput := io.cacheInput(0)
  io.cacheOutput(0) := l1s(0).io.cacheOutput
  l1s(0).io.busIn := l1s(1).io.busOut
  l1s(0).io.busValid := l1s(1).io.validateBus

  l1s(1).io.procOp := io.procOp(1)
  l1s(1).io.prAddr := io.addr(1)
  l1s(1).io.cacheInput := io.cacheInput(1)
  io.cacheOutput(1) := l1s(1).io.cacheOutput
  l1s(1).io.busIn := l1s(0).io.busOut
  l1s(1).io.busValid := l1s(0).io.validateBus
}

object MESITop extends App {
  (new ChiselStage).emitVerilog(new MESITop(), Array("--target-dir", "Verilog/MESI"))
}