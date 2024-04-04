package MOESI_Diplomacy_Decoupled

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class Memory(val ps: MESIPS)(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new MemModuleImp(this)
  val memNode = Seq.fill(procNum)(new MemoryNode(Seq(ps)))

  class MemModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = memNode.head.edges.in.head

    val busIn = memNode.map(_.in.head._1.masterOut.bits.busData).head
    val wen = memNode.map(_.in.head._1.masterOut.bits.flag).head

    val busResp = memNode.map(_.in.head._1.masterIn.bits.busData)
    memNode.foreach(_.in.foreach(_._1.masterIn.bits.flag := false.B))

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
