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
    val prData = Input(Vec(procNum, UInt(cacheBlockBits.W)))
    val cacheOutput = Output(Vec(procNum, UInt(cacheBlockBits.W)))


    // DEBUG info
    val busData = Output(new BusData)
    val memory = Output(new BusData)
    val replFlag = Output(new Bool)
    val busAddr = Output(UInt(addrBits.W))
    val busValid = Output(new Bool)
    val procValid = Output(new Bool)

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
    l1s(i).io.prData := io.prData(i)
    l1s(i).io.busIn := bus.io.l1CachesOut(i)
    bus.io.l1CachesIn(i) := l1s(i).io.busOut
    bus.io.validateBus(i) := l1s(i).io.validateBus
    l1s(i).io.busReplFlag := bus.io.replFlag(i)

    // Verify interface
    io.cacheStatus(i) := l1s(i).io.cacheStatus
    prBundle(i) := Cat(l1s(i).io.procOp, l1s(i).io.prAddr, l1s(i).io.prData).asUInt
  }

  def generateAssert(addr: UInt): Unit = {
    val (tag, index) = parseAddr(addr)

    val replacing = bus.io.l1CachesOut(0).valid && bus.io.l1CachesOut(0).busTransaction === Repl

    // the status of the addr in every cache
    val match_tag_status = l1s.map { l1 =>
      Mux(l1.io.tagDirectory(index) === tag, l1.io.cacheStatus(index), Invalidated)
    }
    // the number of caches of addr for each status
    val exclusive = PopCount(match_tag_status.map(_ === Exclusive))
    assert(exclusive <= 1.U || replacing)
    val modified = PopCount(match_tag_status.map(_ === Modified))
    assert(modified <= 1.U || replacing)
    val owned = PopCount(match_tag_status.map(_ === Owned))
    assert(owned <= 1.U || replacing)
    val shared = PopCount(match_tag_status.map(_ === Shared))
    assert(exclusive === 0.U || modified + owned + shared === 0.U || replacing)
    assert(modified === 0.U || exclusive + owned + shared === 0.U || replacing)
    assert(shared === 0.U || owned === 1.U || replacing)

    // the data vector of the cache lines of index
    val l1Data = VecInit(l1s.map(_.io.cacheData(index)))

    // choose a valid cache of this addr
    val pivot = MuxCase(procNum.U(procNumBits.W),
      match_tag_status.map(_ =/= Invalidated).zipWithIndex.map {
        case (status, i) =>
          status -> i.U(procNumBits.W)
      })
    val pivot_data = Mux(pivot < procNum.U, l1Data(pivot), 0.U)

    // all the other caches of addr must have the same data
    val match_tag_data = match_tag_status.zip(l1Data).zipWithIndex.map {
      case ((status, data), i) =>
        Mux(status =/= Invalidated && pivot =/= i.U(procNumBits.W),
          Mux(data === pivot_data, true.B, false.B),
          true.B
        )
    }
    assert(pivot === procNum.U || match_tag_data.reduce(_ && _))
  }

  (0 until 1 << addrBits).foreach { addr =>
    generateAssert(addr.U(addrBits.W))
  }

  bus.io.memIn := mem.io.busResp
  mem.io.busIn := bus.io.memOut
  mem.io.wen := bus.io.memWen

  // DEBUG info
  io.busData := bus.io.l1CachesOut(1)
  io.memory := mem.io.busResp
  io.replFlag := bus.io.replFlag(1)
  io.busValid := bus.io.valid
  io.procValid := l1s(1).io.validateBus
  io.busAddr := bus.io.l1CachesOut(1).addr
  io.memAddr := mem.io.memAddr
}

object MOESITop extends App {
  (new ChiselStage).emitVerilog(new MOESITop(), Array("--target-dir", "generated/MOESI"))
  //  Check.bmc(() => new MOESITop)
}