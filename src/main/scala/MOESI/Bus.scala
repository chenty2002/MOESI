package MOESI

import Util.RoundRobinArbiter
import chisel3._
import chisel3.util._

class Bus extends Module with HasMOESIParameters {
  val io = IO(new Bundle() {
    val l1CachesIn = Input(Vec(procNum, new BusData))
    val l1CachesOut = Output(Vec(procNum, new BusData))

    val memIn = Input(new BusData)
    val memOut = Output(new BusData)
    val memWen = Output(new Bool)

    val ackHold = Output(new Bool)
    val hold = Output(new Bool)

    val validateBus = Input(Vec(procNum, new Bool))

    // DEBUG info
    val valid = Output(new Bool)
  })

  val arbiter = Module(new RoundRobinArbiter(procNum))
  arbiter.io.requests := io.validateBus

  val valid = io.validateBus.reduce(_ || _)
  io.valid := valid

  val memHold = RegInit(false.B)
  val flushHold = RegInit(false.B)
  val ackHold = RegInit(false.B)
  val busData = RegInit(0.U.asTypeOf(new BusData))
  val pid = RegInit(0.U(procNumBits.W))
  pid := OHToUInt(arbiter.io.grant)
  val memData = RegInit(0.U.asTypeOf(new BusData))
  val memWen = RegInit(false.B)

  val memHoldFlag = RegInit(false.B)
  // indicating the bus is waiting for a response/ack from the memory/another cache
  io.hold := memHold || flushHold || ackHold
  io.ackHold := ackHold

  //  pid := io.pidIn
  //  io.pidOut := pid
  io.memWen := memWen
  memData := io.memIn

  printf("bus: Stage 1\n")
  when(memHold || flushHold || ackHold) {
    printf("bus: Stage 2\n")
    // waiting for the ack from other caches
    when(ackHold) {
      when(busData.busTransaction === BusUpgrade || busData.busTransaction === BusRdX) {
        when(io.l1CachesIn.map(_.busTransaction === Ack).reduce(_ || _)) {
          ackHold := false.B
        }
      }
    }
    // waiting for the response from the memory
    when(memHold && memData.valid && busData.valid && memData.addr === busData.addr) {
      printf("bus: Stage 3\n")
      when(memData.busTransaction === BusUpgrade) {
        memHold := false.B
        memWen := false.B
      }.otherwise {
        when(memHoldFlag) {
          // copy the info of the memory (ignores pid because the response needs to correspond to the request)
          busData.busTransaction := memData.busTransaction
          busData.cacheBlock := memData.cacheBlock
          memHold := false.B
          memHoldFlag := false.B
        }.otherwise {
          memHoldFlag := true.B
        }
      }
    }
    // waiting for the response from other caches
    when(flushHold) {
      printf("bus: Stage 4\n")
      flushHold := false.B
      val flushFlag = io.l1CachesIn.map { l1 =>
        l1.valid &&
          l1.addr === busData.addr &&
          l1.busTransaction === Flush
      }
      printf("bus: flushFlag (%d, %d, %d, %d)\n",
        flushFlag(0), flushFlag(1), flushFlag(2), flushFlag(3))
      when(!flushFlag.reduce(_ || _)) {
        printf("bus: Stage 5\n")
        memHold := true.B
      }.otherwise {
        printf("bus: Stage 6\n")
        // get the pid of the responded cache
        val tarPid = MuxCase(procNum.U(procNumBits.W),
          flushFlag.zipWithIndex.map { flush =>
            flush._1 -> flush._2.U(procNumBits.W)
          })
        printf("bus: TarPid: %d\n", tarPid)
        when(tarPid =/= procNum.U(procNumBits.W)) {
          printf("bus: Stage 7\n")
          busData := io.l1CachesIn(tarPid)
        }
      }
    }
  }.otherwise {
    memWen := false.B
    busData := io.l1CachesIn(pid)
    when(valid) {
      printf("bus: Stage 8\n")

      when(busData.busTransaction === BusRd) {
        printf("bus: Stage 9\n")
        flushHold := true.B
      }.elsewhen(busData.busTransaction === BusUpgrade) {
        printf("bus: Stage 10\n")
        memHold := true.B
        memWen := true.B
      }
    }.otherwise {
      printf("bus: Stage 11\n")
      busData := 0.U.asTypeOf(new BusData)
    }
  }
  io.memOut := busData
  io.l1CachesOut.foreach(_ := busData)
  printf("bus: Hold: %d\n", memHold || flushHold)
}
