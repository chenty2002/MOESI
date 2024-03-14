package MOESI_Diplomacy

import chisel3._
import chisel3.experimental.SourceInfo
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

import scala.math._

class MESIPS(val tagB: Int, val indexB: Int, val blockB: Int) {
  val tagBits: Int = tagB
  val indexBits: Int = indexB
  val cacheBlockBits: Int = blockB

  val addrBits: Int = tagBits + indexBits

  def parseAddr(addr: UInt): (UInt, UInt) = {
    val indexStart = 0
    val indexEnd = indexStart + indexBits - 1
    val index = addr(indexEnd, indexStart)
    val tagStart = indexEnd
    val tagEnd = tagStart + tagBits - 1
    val tag = addr(tagEnd, tagStart)
    (tag, index)
  }
}

object MESINodeImp extends SimpleNodeImp[MESIPS, MESIPS, MESIPS, BusBundle] {
  override def edge(pd: MESIPS, pu: MESIPS, p: Parameters, sourceInfo: SourceInfo): MESIPS = {
    new MESIPS(
      min(pd.tagB, pu.tagB),
      min(pd.indexB, pu.indexB),
      min(pd.blockB, pu.blockB)
    )
  }

  override def bundle(e: MESIPS): BusBundle = {
    new BusBundle(e)
  }

  override def render(e: MESIPS): RenderedEdge = RenderedEdge("gray")
}

class L1SourceNode(dps: Seq[MESIPS])(implicit valName: ValName)
  extends SourceNode(MESINodeImp)(dps)

class L1SinkNode(ups: Seq[MESIPS])(implicit valName: ValName)
  extends SinkNode(MESINodeImp)(ups)

class BusDNode(dFn: Seq[MESIPS] => MESIPS,
               uFn: Seq[MESIPS] => MESIPS)(implicit valName: ValName)
  extends NexusNode(MESINodeImp)(dFn, uFn)

class BusUNode(dFn: Seq[MESIPS] => MESIPS,
               uFn: Seq[MESIPS] => MESIPS)(implicit valName: ValName)
  extends NexusNode(MESINodeImp)(dFn, uFn)

class MemorySinkNode(ups: Seq[MESIPS])(implicit valName: ValName)
  extends SinkNode(MESINodeImp)(ups)

class MemorySourceNode(dps: Seq[MESIPS])(implicit valName: ValName)
  extends SourceNode(MESINodeImp)(dps)