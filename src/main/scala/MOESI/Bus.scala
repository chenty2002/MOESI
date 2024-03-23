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
    val replFlag = Output(new Bool)

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
  val busDataBuffer = RegInit(0.U.asTypeOf(new BusData))
  val replDataBuffer = RegInit(0.U.asTypeOf(new BusData))
  val pid = RegInit(procNum.U(procNumBits.W))
  val processing = RegInit(false.B)
  val memData = RegInit(0.U.asTypeOf(new BusData))
  val memWen = RegInit(false.B)

  val memHoldFlag = RegInit(false.B)
  val flushCleanFlag = RegInit(false.B)
  // indicating the bus is waiting for a response/ack from the memory/another cache
  //  io.hold := memHold || flushHold || ackHold
  io.replFlag := false.B
  io.ackHold := ackHold

  def enableIO(): Unit = {
    processing := false.B
    busDataBuffer := 0.U.asTypeOf(new BusData)
    busData := 0.U.asTypeOf(new BusData)
  }

  // bus buffer, take the arbiter output only when the last transaction is done
  when(processing) {
    busData := busDataBuffer
  }
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
          printf("bus: Stage 3\n")
          when(!memHold && !ackHold) {
            enableIO()
          }
        }
      }
    }
    // waiting for the response from the memory
    when(memHold && memData.valid && busData.valid && memData.addr === busData.addr) {
      printf("bus: Stage 4\n")
      when(memData.busTransaction === BusUpgrade || memData.busTransaction === Repl) {
        printf("bus: Stage 5\n")
        memHold := false.B
        memWen := false.B
        when(memData.busTransaction === Repl) {
          io.replFlag := true.B
        }
        when(!flushHold && !ackHold) {
          enableIO()
        }
      }.otherwise {
        // Fill transaction needs to wait one more beat for the cache to process
        when(memHoldFlag) {
          printf("bus: Stage 6\n")
          memHold := false.B
          memHoldFlag := false.B
          when(!flushHold && !ackHold) {
            enableIO()
          }
        }.otherwise {
          printf("bus: Stage 7\n")
          // copy the info of the memory (ignores pid because the response needs to correspond to the request)
          busData.busTransaction := memData.busTransaction
          busData.cacheBlock := memData.cacheBlock
          memHoldFlag := true.B
        }
      }
    }
    // waiting for the response from other caches
    when(flushHold) {
      printf("bus: Stage 8\n")
      flushHold := false.B
      val flushFlag = io.l1CachesIn.map { l1 =>
        l1.valid &&
          l1.addr === busData.addr &&
          l1.busTransaction === Flush
      }
      printf("bus: flushFlag (%d, %d, %d, %d)\n",
        flushFlag(0), flushFlag(1), flushFlag(2), flushFlag(3))
      when(!flushFlag.reduce(_ || _)) {
        when(busData.busTransaction =/= Repl) {
          printf("bus: Stage 9\n")
          memHold := true.B
        }
      }.otherwise {
        printf("bus: Stage 11\n")
        val hasShared = io.l1CachesIn.zip(flushFlag).map { l1 =>
          l1._1.state === Shared && l1._2
        }.reduce(_ || _)
        // get the pid of the responded cache
        val tarPid = MuxCase(procNum.U(procNumBits.W),
          flushFlag.zipWithIndex.map { flush =>
            flush._1 -> flush._2.U(procNumBits.W)
          })
        printf("bus: TarPid: %d\n", tarPid)
        when(tarPid =/= procNum.U(procNumBits.W)) {
          when(busData.busTransaction === Repl) {
            // replacing Owned cache, choose a Shared cache to become Owned
            when(busData.state === Owned && io.l1CachesIn(tarPid).state === Shared) {
              printf("bus: Stage 12\n")
              busData.pid := tarPid
              flushCleanFlag := true.B
            }.elsewhen(!hasShared) {
              // bus replace request, inform the Owned cache to become Modified
              // special usage
              busData.state := Invalidated
              flushCleanFlag := true.B
            }
          }.otherwise {
            printf("bus: Stage 13\n")
            busData := io.l1CachesIn(tarPid)
          }
        }
        // the flush data on the bus needs to be cleared, but it needs to last one more beat
        when(!memHold && !ackHold) {
          flushCleanFlag := true.B
        }
      }
    }
  }.otherwise {
    printf("bus: Stage 14\n")
    memWen := false.B
    when(flushCleanFlag) {
      when(busData.busTransaction === Repl) {
        io.replFlag := true.B
      }
      enableIO()
      flushCleanFlag := false.B
    }.otherwise {
      val hasRepl = io.l1CachesIn.map { l1 =>
        l1.valid && l1.busTransaction === Repl
      }
      when(busDataBuffer.busTransaction =/= Repl && hasRepl.reduce(_ || _)) {
        val replPid = MuxCase(procNum.U(procNumBits.W),
          hasRepl.zipWithIndex.map { repl =>
            repl._1 -> repl._2.U(procNumBits.W)
          })
        busDataBuffer := io.l1CachesIn(replPid)
      }.elsewhen(busDataBuffer.valid) {
        processing := true.B
        busDataBuffer := busDataBuffer
      }.otherwise {
        busDataBuffer := io.l1CachesIn(OHToUInt(arbiter.io.grant))
      }
    }
    when(busData.valid) {
      printf("bus: Stage 15\n")
      when(busData.busTransaction === BusRd) {
        printf("bus: Stage 16\n")
        flushHold := true.B
      }.elsewhen(busData.busTransaction === BusUpgrade) {
        printf("bus: Stage 17\n")
        memHold := true.B
        memWen := true.B
      }.elsewhen(busData.busTransaction === BusRdX) {
        printf("bus: Stage 18\n")
        enableIO()
      }.elsewhen(busData.busTransaction === Repl) {
        printf("bus: Stage 19\n")
        switch(busData.state) {
          is(Modified) {
            // replacing Modified cache, only needs to write the data to memory
            memHold := true.B
            memWen := true.B
          }
          is(Owned) {
            // replacing Owned cache, there is at least one Shared cache,
            // choose one Shared cache to become Owned
            flushHold := true.B
          }
          is(Shared) {
            // replacing Shared cache, there must be one Owned cache,
            // if there are no other Shared caches, the Owned cache needs to become Modified
            // if there exists another Shared cache, do nothing
            flushHold := true.B
          }
        }
      }
    }
  }
  io.memOut := busData
  io.l1CachesOut.foreach(_ := busData)
  printf("bus: Hold: %d\n", memHold || flushHold)
}
