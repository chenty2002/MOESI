package MOESI_Diplomacy

import Util.File
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import chiselFv._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class MOESITop(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
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

  bus.busDNode.zip(l1s.map(_.l1DNode)).foreach { nodes =>
    nodes._1 := nodes._2
  }
  bus.busDNode.zip(mem.memDNode).foreach { nodes =>
    nodes._2 := nodes._1
  }
  bus.busUNode.zip(l1s.map(_.l1UNode)).foreach { nodes =>
    nodes._2 := nodes._1
  }
  bus.busUNode.zip(mem.memUNode).foreach { nodes =>
    nodes._1 := nodes._2
  }

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
        Mux(l1.module.io.tagDirectory(index) === tag, l1.module.io.cacheStatus(index), Invalidated)
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
      assert(pivot === procNum.U || match_tag_data.reduce(_ && _) || replacing)
    }

    (0 until 1 << ep.addrBits).foreach { addr =>
      generateAssert(addr.U(ep.addrBits.W))
    }
  }
}

object MOESITop extends App {
  val lazyModule = LazyModule(new MOESITop()(Parameters.empty))
  (new ChiselStage).emitVerilog(lazyModule.module, Array("--target-dir", "generated/MOESI_Diplomacy"))
  File.writeOutputFile("generated/MOESI_Diplomacy", "MOESI_Diplomacy.graphml", lazyModule.graphML)
//  Check.bmc(() => LazyModule(new MOESITop()(Parameters.empty)).module, 30)
}