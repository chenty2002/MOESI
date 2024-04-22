package MOESI_Diplomacy_Unified

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class Memory(val ps: MOESIPS)(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new MemModuleImp(this)
  val memNode = new MemoryNode(Seq(ps))

  class MemModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = memNode.edges.in.head

    val busIn = memNode.in.head._1.masterOut.busData
    val wen = memNode.in.head._1.masterOut.flag

    val busResp = memNode.in.head._1.masterIn.busData
    memNode.in.foreach(_._1.masterIn.flag := false.B)

    val mem = SyncReadMem(1 << ep.addrBits, UInt(ep.cacheBlockBits.W))

    when(busIn.valid) {
      val addr = Cat(busIn.addrBundle.tag, busIn.addrBundle.index)
      val wr = busIn.addrBundle.cacheBlock
      busResp := busIn

      val transEn = busIn.busTransaction === BusUpgrade ||
        (busIn.busTransaction === Repl && busIn.state === Modified)
      when(wen && transEn) {
        mem.write(addr, wr)
        busResp.addrBundle.cacheBlock := wr
        when(busIn.busTransaction === BusUpgrade || busIn.busTransaction === Repl) {
          busResp.busTransaction := busIn.busTransaction
        }.otherwise {
          busResp.busTransaction := Invalid
        }
      }.otherwise {
        busResp.addrBundle.cacheBlock := mem.read(addr)
        busResp.busTransaction := Fill
      }
    }.otherwise {
      busResp := 0.U.asTypeOf(new BusData(ep))
    }
  }
}
