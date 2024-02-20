package MESI

import chisel3._
import chisel3.util._
import MESI.Arbiter

class Bus extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val l1CachesIn = Input(Vec(procNum, new BusData))
    val l1CachesOut = Output(Vec(procNum, new BusData))

    val memIn = Input(new BusData)
    val memOut = Output(new BusData)
    val memWen = Output(new Bool)

    val validateBus = Input(Vec(procNum, new Bool))
  })

  val arbiter = Module(new Arbiter)
  val arbiterIn = WireDefault(io.validateBus)

//  def filterSharedFlush(l1data: Vec[BusData], validates: Vec[Bool]): Vec[Bool] = {
//
//    for(i <- 0 until procNum) {
//      when(l1data(i).valid && l1data(i).pid =/= 0.U && validates(i)) {
//
//      }
//    }
//  }

  arbiter.io.requests := io.validateBus

  val valid = WireDefault(false.B)
//  io.busValid.foreach(_ := valid)
  valid := io.validateBus.reduce(_ || _)

  val pid = RegInit(0.U(procNumBits.W))
  val busData = RegInit(0.U.asTypeOf(new BusData))
  val memData = RegInit(0.U.asTypeOf(new BusData))
  val procResp = RegInit(VecInit.fill(procNum)(0.U.asTypeOf(new BusData)))

//  pid := io.pidIn
//  io.pidOut := pid
  pid := OHToUInt(arbiter.io.grant)
  io.memWen := false.B
  memData := io.memIn
  when(valid) {
    busData := io.l1CachesIn(pid)
    io.l1CachesOut.foreach(_ := busData)
    io.memOut := busData

    switch(busData.busTransaction) {
      is(BusRd) {
        val flush = io.l1CachesIn.map { l1 =>
          busData.pid =/= l1.pid &&
            l1.valid &&
            l1.addr === busData.addr &&
            l1.busTransaction === Flush
        }.reduce(_ || _)
        when(!flush) {
          busData := memData
        }
      }
      is(Flush) {
        when(busData.state === Modified) {
          io.memWen := true.B
        }
      }
    }
  }.otherwise {
    busData := 0.U.asTypeOf(new BusData)
    io.memOut := 0.U.asTypeOf(new BusData)
    io.l1CachesOut.foreach(_ := 0.U.asTypeOf(new BusData))
  }
}
