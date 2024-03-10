package MESI

import chisel3._
import chisel3.util._

class Arb(val grantN: Int) extends Module {
  val io = IO(new Bundle() {
    val requests = Input(Vec(grantN, new Bool))
    val grant = Output(Vec(grantN, new Bool))
  })

  val priority = RegInit(1.U(grantN.W))
  val priorityPre = RegInit(1.U(grantN.W))
  priorityPre := priority
  val grant = Wire(UInt(grantN.W))
  val grantNext = WireDefault(0.U((2*grantN).W))
  when(io.requests.reduce(_ || _) && priority =/= priorityPre) {
    priority := Cat(grant(grantN-2, 0), grant(grantN-1))
  }
  val cat = Cat(io.requests.asUInt, io.requests.asUInt)

  grantNext := cat & (~(cat - Cat(0.U(grantN.W), priority))).asUInt
  grant := grantNext(grantN-1, 0) | grantNext(2*grantN-1, grantN)
  io.grant := grant.asBools
}
