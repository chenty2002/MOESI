package MOESI_Diplomacy

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class Memory(val ps: MESIPS)(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new MemModuleImp(this)
  val memDNode = Seq.fill(procNum)(new MemorySinkNode(Seq(ps)))
  val memUNode = Seq.fill(procNum)(new MemorySourceNode(Seq(ps)))

  class MemModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = memUNode.head.edges.out.head

    val busIn = memDNode.map(_.in.head._1.busData).head
    val wen = memDNode.map(_.in.head._1.flag).head

    val busResp = memUNode.map(_.out.head._1.busData)
    memUNode.foreach(_.out.foreach(_._1.flag := false.B))

    val mem = SyncReadMem(1 << ep.addrBits, UInt(ep.cacheBlockBits.W))

    when(busIn.valid) {
      val addr = Cat(busIn.addrBundle.tag, busIn.addrBundle.index)
      val wr = busIn.addrBundle.cacheBlock
      busResp.foreach(_ := busIn)

      val transEn = busIn.busTransaction === BusUpgrade ||
        (busIn.busTransaction === Repl && busIn.state === Modified)
      when(wen && transEn) {
        mem.write(addr, wr)
        busResp.foreach(_.addrBundle.cacheBlock := wr)
        when(busIn.busTransaction === BusUpgrade || busIn.busTransaction === Repl) {
          busResp.foreach(_.busTransaction := busIn.busTransaction)
        }.otherwise {
          busResp.foreach(_.busTransaction := Invalid)
        }
      }.otherwise {
        busResp.foreach(_.addrBundle.cacheBlock := mem.read(addr))
        busResp.foreach(_.busTransaction := Fill)
      }
    }.otherwise {
      busResp.foreach(_ := 0.U.asTypeOf(new BusData(ep)))
    }
  }
}
