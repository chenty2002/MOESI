package MOESI_Diplomacy_Unified

import Util.File
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import chiselFv._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class MOESIDiplomacyUnifiedTop(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  /*
  proc0   proc1   proc2   proc3
    |       |       |       |
 l1cache l1cache l1cache l1cache
     \      \       /       /
---------B------U-------S--------
                |
              memory
   */
  val l1s = (0 until procNum).map(i => LazyModule(
    new L1Cache(
      hostPid = i.U(procNumBits.W),
      new MESIPS(2, 2, 8)
    )))
  val mem = LazyModule(new Memory(new MESIPS(2, 2, 8)))
  val bus = LazyModule(new Bus)

  l1s.foreach(bus.busNode := _.l1Node)
  mem.memNode := bus.busNode

  lazy val module = new MESITopModule(this)

  class MESITopModule(wrapper: LazyModule) extends LazyModuleImp(wrapper) with Formal {
    val io = IO(new Bundle() {
      val procOp = Input(Vec(procNum, UInt(procOpBits.W)))
      val procResp = Output(Vec(procNum, new Bool))
      val addr = Input(Vec(procNum, UInt(4.W)))
      val prData = Input(Vec(procNum, UInt(8.W)))
      val cacheOutput = Output(Vec(procNum, UInt(8.W)))
    })

    for (i <- 0 until procNum) {
      l1s(i).module.io.procOp := io.procOp(i)
      io.procResp(i) := l1s(i).module.io.response
      l1s(i).module.io.prAddr := io.addr(i)
      io.cacheOutput(i) := l1s(i).module.io.cacheOutput
      l1s(i).module.io.prData := io.prData(i)
    }

    val ep = new MESIPS(2, 2, 8)

    def generateAssert(addr: UInt): Unit = {
      val (tag, index) = ep.parseAddr(addr)

      val replacing = bus.module.verify_io.replacing

      // the status of the addr in every cache
      val match_tag_status = l1s.map { l1 =>
        Mux(l1.module.io.tagDirectory(index) === tag,
          l1.module.io.cacheStatus(index),
          Invalidated)
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
      val l1Data = VecInit(l1s.map(_.module.io.cacheData(index)))
      // the Owned cache line
      val ownedData = l1Data.zipWithIndex.map {
        case (data, i) =>
          Mux(match_tag_status(i) === Owned, data, 0.U)
      }.reduce(_ + _)
      // all the Shared cache line must equal the Owned cache line
      l1Data.zipWithIndex.foreach {
        case (data, i) =>
          when(match_tag_status(i) === Shared) {
            assert(data === ownedData || replacing)
          }
      }
    }

    (0 until 1 << ep.addrBits).foreach { addr =>
      generateAssert(addr.U(ep.addrBits.W))
    }
  }
}

object MOESIDiplomacyUnifiedTop extends App {
  val lazyModule = LazyModule(new MOESIDiplomacyUnifiedTop()(Parameters.empty))
  (new ChiselStage).emitSystemVerilog(lazyModule.module, Array("--target-dir", "generated/MOESI_Diplomacy_Decoupled"))
  File.writeOutputFile(
    "generated/MOESI_Diplomacy_Decoupled",
    "MOESI_Diplomacy_Decoupled.graphml",
    lazyModule.graphML)
  //  Check.bmc(() => LazyModule(new MOESITop()(Parameters.empty)).module, 30)
}