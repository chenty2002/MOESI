package MESI_Diplomacy

import Util.File
import chisel3._
import chisel3.stage.ChiselStage
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class MESITop(implicit p: Parameters) extends LazyModule with HasMESIParameters {
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
  class MESITopModule(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle() {
      val procOp = Input(Vec(procNum, UInt(procOpBits.W)))
      val procHlt = Output(Vec(procNum, new Bool))
      val addr = Input(Vec(procNum, UInt(4.W)))
      val cacheInput = Input(Vec(procNum, UInt(8.W)))
      val cacheOutput = Output(Vec(procNum, UInt(8.W)))
    })

    for (i <- 0 until procNum) {
      l1s(i).module.io.procOp := io.procOp(i)
      io.procHlt(i) := l1s(i).module.io.prHlt
      l1s(i).module.io.prAddr := io.addr(i)
      io.cacheOutput(i) := l1s(i).module.io.cacheOutput
      l1s(i).module.io.cacheInput := io.cacheInput(i)
    }
  }
}

object MESITop extends App {
  val lazyModule = LazyModule(new MESITop()(Parameters.empty))
  (new ChiselStage).emitVerilog(lazyModule.module, Array("--target-dir", "generated/MESI_Diplomacy"))
  File.writeOutputFile("generated/MESI_Diplomacy", "MESI_Diplomacy.graphml", lazyModule.graphML)
}