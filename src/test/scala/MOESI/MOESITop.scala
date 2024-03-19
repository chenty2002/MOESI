package MOESI

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import chiselFv._

class MOESITop() extends Module with HasMOESIParameters with Formal {
  /*
  proc0   proc1   proc2   proc3
    |       |       |       |
 l1cache l1cache l1cache l1cache
     \      \       /       /
---------B------U-------S--------
                |
              memory
   */

  val io = IO(new Bundle() {
    val procOp = Input(Vec(procNum, UInt(procOpBits.W)))
    val procResp = Output(Vec(procNum, new Bool))
    val addr = Input(Vec(procNum, UInt(addrBits.W)))
    val cacheInput = Input(Vec(procNum, UInt(cacheBlockBits.W)))
    val cacheOutput = Output(Vec(procNum, UInt(cacheBlockBits.W)))


    // DEBUG info
    val busData = Output(new BusData)
    val memory = Output(new BusData)
    val busHold = Output(new Bool)
    val busAddr = Output(UInt(addrBits.W))
    val busValid = Output(new Bool)
    val procValid = Output(new Bool)

    val addrEq = Output(new Bool)
    val cacheStatus = Output(Vec(procNum, Vec(cacheBlockNum, UInt(stateBits.W))))
    val memAddr = Output(UInt(addrBits.W))
  })

  val l1s = (0 until procNum).map(i => Module(new L1Cache(hostPid = i.U(procNumBits.W))))
  val mem = Module(new Memory)
  val bus = Module(new Bus)

  val prBundle = Wire(Vec(procNum, UInt((procOpBits + addrBits + cacheBlockBits).W)))

  for (i <- 0 until procNum) {
    l1s(i).io.procOp := io.procOp(i)
    io.procResp(i) := l1s(i).io.response
    l1s(i).io.prAddr := io.addr(i)
    io.cacheOutput(i) := l1s(i).io.cacheOutput
    l1s(i).io.cacheInput := io.cacheInput(i)
    l1s(i).io.busIn := bus.io.l1CachesOut(i)
    bus.io.l1CachesIn(i) := l1s(i).io.busOut
    bus.io.validateBus(i) := l1s(i).io.validateBus
    l1s(i).io.busHold := bus.io.hold
    l1s(i).io.busAckHold := bus.io.ackHold

    // DEBUG info
    io.cacheStatus(i) := l1s(i).io.cacheStatus
    prBundle(i) := Cat(l1s(i).io.procOp, l1s(i).io.prAddr, l1s(i).io.cacheInput).asUInt
  }

  def generateAssert(ast: (Bool, String) => Unit, addr: UInt): Unit = {
    val (tag, index) = parseAddr(addr)
    val match_tag = l1s.map { l1 =>
      Mux(l1.io.tagDirectory(index) === tag, l1.io.cacheStatus(index), Invalidated)
    }
    val exclusive = PopCount(match_tag.map(_ === Exclusive))
    ast(exclusive <= 1.U, "")
    val modified = PopCount(match_tag.map(_ === Modified))
    ast(modified <= 1.U, "")
    val owned = PopCount(match_tag.map(_ === Owned))
    ast(owned <= 1.U, "")
    val shared = PopCount(match_tag.map(_ === Shared))
    ast(exclusive === 0.U || modified + owned + shared === 0.U, "")
    ast(modified === 0.U || exclusive + owned + shared === 0.U, "")
    ast(shared === 0.U || owned === 1.U, "")
  }

  def inputValid(cond: Bool, s: String = ""): Unit = {
    for (i <- 0 until procNum) {
      past(prBundle(i), 1) { pastIO =>
        assert(cond ||
          !(!l1s(i).io.prHlt ||
            prBundle(i) === pastIO), s)
      }
    }
  }

//  (0 until 1 << addrBits).foreach { addr =>
//    generateAssert(assert, addr.U(addrBits.W))
//  }

  bus.io.memIn := mem.io.busResp
  mem.io.busIn := bus.io.memOut
  mem.io.wen := bus.io.memWen

  // DEBUG info
  io.busData := bus.io.l1CachesOut(1)
  io.memory := mem.io.busResp
  io.busHold := bus.io.hold
  io.busValid := bus.io.valid
  io.procValid := l1s(1).io.validateBus
  io.busAddr := bus.io.l1CachesOut(1).addr
  io.addrEq := l1s(1).io.addrEq
  io.memAddr := mem.io.memAddr
}

object MOESITop extends App {
  (new ChiselStage).emitVerilog(new MOESITop(), Array("--target-dir", "generated/MOESI"))
  //  Check.bmc(() => new MOESITop)
}