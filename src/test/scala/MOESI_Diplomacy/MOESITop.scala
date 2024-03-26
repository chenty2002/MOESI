package MOESI_Diplomacy

import Util.File
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.PopCount
import chiselFv.{Check, Formal}
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
      val procHlt = Output(Vec(procNum, new Bool))
      val addr = Input(Vec(procNum, UInt(4.W)))
      val prData = Input(Vec(procNum, UInt(8.W)))
      val cacheOutput = Output(Vec(procNum, UInt(8.W)))
    })

    for (i <- 0 until procNum) {
      l1s(i).module.io.procOp := io.procOp(i)
      io.procHlt(i) := l1s(i).module.io.prHlt
      l1s(i).module.io.prAddr := io.addr(i)
      io.cacheOutput(i) := l1s(i).module.io.cacheOutput
      l1s(i).module.io.prData := io.prData(i)
    }

    val ep = new MESIPS(2, 2, 8)

    def generateAssert(addr: UInt): Unit = {
      val (tag, index) = ep.parseAddr(addr)
      val match_tag = l1s.map { l1 =>
        Mux(l1.module.io.tagDirectory(index) === tag, l1.module.io.cacheStatus(index), Invalidated)
      }
      val exclusive = PopCount(match_tag.map(_ === Exclusive))
      assert(exclusive <= 1.U)
      val modified = PopCount(match_tag.map(_ === Modified))
      assert(modified <= 1.U)
      val owned = PopCount(match_tag.map(_ === Owned))
      assert(owned <= 1.U)
      val shared = PopCount(match_tag.map(_ === Shared))
      assert(exclusive === 0.U || modified + owned + shared === 0.U)
      assert(modified === 0.U || exclusive + owned + shared === 0.U)
      assert(shared === 0.U || owned === 1.U)
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
  Check.bmc(() => LazyModule(new MOESITop()(Parameters.empty)).module)
}