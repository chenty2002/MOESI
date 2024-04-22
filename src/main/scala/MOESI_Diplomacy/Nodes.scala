package MOESI_Diplomacy

import chisel3._
import chisel3.experimental.SourceInfo
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

import scala.math._

class MOESIPS(val tagB: Int, val indexB: Int, val blockB: Int) {
  val tagBits: Int = tagB
  val indexBits: Int = indexB
  val cacheBlockBits: Int = blockB

  val addrBits: Int = tagBits + indexBits

  def parseAddr(addr: UInt): (UInt, UInt) = {
    val indexStart = 0
    val indexEnd = indexStart + indexBits - 1
    val index = addr(indexEnd, indexStart)
    val tagStart = indexEnd + 1
    val tagEnd = tagStart + tagBits - 1
    val tag = addr(tagEnd, tagStart)
    (tag, index)
  }
}

object MOESINodeImp extends SimpleNodeImp[MOESIPS, MOESIPS, MOESIPS, BusBundle] {
  override def edge(pd: MOESIPS, pu: MOESIPS, p: Parameters, sourceInfo: SourceInfo): MOESIPS = {
    new MOESIPS(
      min(pd.tagB, pu.tagB),
      min(pd.indexB, pu.indexB),
      min(pd.blockB, pu.blockB)
    )
  }

  override def bundle(e: MOESIPS): BusBundle = {
    new BusBundle(e)
  }

  override def render(e: MOESIPS): RenderedEdge = RenderedEdge("gray")
}

class L1SourceNode(dps: Seq[MOESIPS])(implicit valName: ValName)
  extends SourceNode(MOESINodeImp)(dps)

class L1SinkNode(ups: Seq[MOESIPS])(implicit valName: ValName)
  extends SinkNode(MOESINodeImp)(ups)

class BusDNode(dFn: Seq[MOESIPS] => MOESIPS,
               uFn: Seq[MOESIPS] => MOESIPS)(implicit valName: ValName)
  extends NexusNode(MOESINodeImp)(dFn, uFn)

class BusUNode(dFn: Seq[MOESIPS] => MOESIPS,
               uFn: Seq[MOESIPS] => MOESIPS)(implicit valName: ValName)
  extends NexusNode(MOESINodeImp)(dFn, uFn)

class MemorySinkNode(ups: Seq[MOESIPS])(implicit valName: ValName)
  extends SinkNode(MOESINodeImp)(ups)

class MemorySourceNode(dps: Seq[MOESIPS])(implicit valName: ValName)
  extends SourceNode(MOESINodeImp)(dps)