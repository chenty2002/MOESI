package MESI

import chisel3._
import chisel3.util._

class Arbiter extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val requests = Input(Vec(procNum, new Bool))
    val grant = Output(Vec(procNum, new Bool))
  })

  val lastState = RegInit(1.U(procNum.W))
  val grant = RegInit(0.U(procNum.W))
  val grantNext = WireDefault(0.U((2*procNum).W))
  when(io.requests.asUInt =/= 0.U) {
    lastState := Cat(grant(procNum-2, 0), grant(procNum-1))
  }
  val cat = Cat(io.requests.asUInt, io.requests.asUInt)

  grantNext := cat & (~(cat - lastState)).asUInt
  grant := grantNext(procNum-1, 0) | grantNext(2*procNum-1, procNum)
  io.grant := grant.asBools
}
