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
       bus
        |
      memory
   */

  val io = IO(new Bundle() {
    val procOp = Input(Vec(procNum, UInt(procOpBits.W)))
    val procHlt = Output(Vec(procNum, new Bool))
    val addr = Input(Vec(procNum, UInt(addrBits.W)))
    val cacheInput = Input(Vec(procNum, UInt(cacheBlockBits.W)))
    val cacheOutput = Output(Vec(procNum, UInt(cacheBlockBits.W)))
//    val memory = Output(Vec(1<<addrBits, UInt(cacheBlockBits.W)))
  })

  val l1s = (1 to procNum).map(i => Module(new L1Cache(hostPid = i.U(procNumBits.W))))
  val mem = Module(new Memory)
  val bus = Module(new Bus)

  for(i <- l1s.indices) {
    l1s(i).io.procOp := io.procOp(i)
    io.procHlt(i) := l1s(i).io.prHlt
    l1s(i).io.prAddr := io.addr(i)
    io.cacheOutput(i) := l1s(i).io.cacheOutput
    l1s(i).io.cacheInput := io.cacheInput(i)
    l1s(i).io.busIn := bus.io.l1CachesOut(i)
    bus.io.l1CachesIn(i) := l1s(i).io.busOut
    bus.io.validateBus(i) := l1s(i).io.validateBus
  }

  bus.io.memIn := mem.io.busResp
  mem.io.busIn := bus.io.memOut
  mem.io.wen := bus.io.memWen
}

object MESITop extends App {
  (new ChiselStage).emitVerilog(new MESITop(), Array("--target-dir", "Verilog/MESI"))
}