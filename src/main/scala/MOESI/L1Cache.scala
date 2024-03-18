package MOESI

import chisel3._
import chisel3.util._

class L1Cache(val hostPid: UInt) extends Module with HasMOESIParameters {
  val io = IO(new Bundle() {
    // processor operations
    val procOp = Input(UInt(procOpBits.W))
    // whether to halt the processor to wait for requests
    val prHlt = Output(new Bool)
    // the address of the operation
    val prAddr = Input(UInt(addrBits.W))

    // the data requests
    val cacheOutput = Output(UInt(cacheBlockBits.W))
    val cacheInput = Input(UInt(cacheBlockBits.W))

    // the requests on the bus
    val busIn = Input(new BusData)
    val busOut = Output(new BusData)
    val busHold = Input(new Bool)
    val busAckHold = Input(new Bool)

    // the cache needs to use the bus
    val validateBus = Output(new Bool)

    // DEBUG info
    val addrEq = Output(new Bool)
    val cacheStatus = Output(Vec(cacheBlockNum, UInt(stateBits.W)))
    val tagDirectory = Output(Vec(cacheBlockNum, UInt(tagBits.W)))
  })
  val prHlt = RegInit(false.B)
  val processing = RegInit(false.B)
  val cacheOutput = WireDefault(0.U(cacheBlockBits.W))
  val busOut = RegInit(0.U.asTypeOf(new BusData))
  val validateBus = RegInit(false.B)

  val prOp = RegInit(0.U(procOpBits.W))
  val prAddr = RegInit(0.U(addrBits.W))
  val prData = RegInit(0.U(cacheBlockBits.W))

  val ioPrOp = RegInit(0.U(procOpBits.W))
  val ioPrAddr = RegInit(0.U(addrBits.W))
  val ioPrData = RegInit(0.U(cacheBlockBits.W))

  val beatCounter = RegInit(0.U(3.W))

  when(hostPid === 0.U) {
    printf("--- 0 ---\n")
  }
  when(ioPrOp === PrWr || ioPrOp === PrRd) {
    when(hostPid === 0.U) {
      printf("--- 1 ---\n")
    }
    processing := true.B
    ioPrOp := ioPrOp
    ioPrAddr := ioPrAddr
    ioPrData := ioPrData
  }.otherwise {
    when(hostPid === 0.U) {
      printf("--- 2 ---\n")
    }
    ioPrOp := io.procOp
    ioPrAddr := io.prAddr
    ioPrData := io.cacheInput
  }
  when(processing) {
    when(hostPid === 0.U) {
      printf("--- 3 ---\n")
    }
    prOp := ioPrOp
    prAddr := ioPrAddr
    prData := ioPrData
  }

  // every operation must stay stable for at least 2 beats for prHlt to take effect
//  when(!prHlt) {
//      when(io.procOp === PrRd || io.procOp === PrWr) {
//        prOp := io.procOp
//        prAddr := io.prAddr
//        prData := io.cacheInput
//      }.otherwise {
//        prOp := 0.U
//        prAddr := 0.U
//        prData := 0.U
//      }
//    beatCounter := 0.U
//  }.otherwise {
//    beatCounter := 0.U
//  }
//  printf("pid %d: counter = %d\n", hostPid, beatCounter)


  io.prHlt := prHlt
  io.validateBus := validateBus
  io.busOut := busOut
  io.cacheOutput := cacheOutput

  val L1Cache = RegInit(VecInit.fill(cacheBlockNum)(0.U(cacheBlockBits.W)))
  val tagDirectory = RegInit(VecInit.fill(cacheBlockNum)(0.U(tagBits.W)))
  val cacheStatus = RegInit(VecInit.fill(cacheBlockNum)(Invalidated))

  val (tag, index) = parseAddr(prAddr)

  // bus requests
  val guestId = io.busIn.pid
  val busTag = io.busIn.tag
  val busTrans = io.busIn.busTransaction
  val busIndex = io.busIn.index
  val busData = io.busIn.cacheBlock
  val busState = io.busIn.state
  val busValid = io.busIn.valid
  val busAddr = io.busIn.addr

  // DEBUG info
  printf("pid %d: BusData\n " +
    "\t\tpid: %d\n " +
    "\t\ttag: %d\n " +
    "\t\ttrans: %d\n " +
    "\t\tindex: %d\n " +
    "\t\tdata: %d\n " +
    "\t\tstate: %d\n " +
    "\t\tvalid: %d\n" +
    "\t\taddr: %d\n",
    hostPid,
    guestId,
    busTag,
    busTrans,
    busIndex,
    busData,
    busState,
    busValid,
    busAddr)

  io.addrEq := busAddr === prAddr
  io.cacheStatus := cacheStatus
  io.tagDirectory := tagDirectory

  def disableIO(): Unit = {
    processing := false.B
    ioPrOp := 0.U
    ioPrAddr := 0.U
    ioPrData := 0.U
    prOp := 0.U
    prAddr := 0.U
    prData := 0.U
  }

  // whether the address hits
  def isHit(t: UInt, i: UInt): Bool = {
    cacheStatus(i) =/= Invalidated && tagDirectory(i) === t
  }

  def fillBus(trans: UInt, t: UInt, ix: UInt, ste: UInt): Unit = {
    fillBus(trans, t, ix, L1Cache(ix), ste)
  }

  def fillBus(trans: UInt, t: UInt, ix: UInt, block: UInt, ste: UInt): Unit = {
    busOut.pid := hostPid
    busOut.busTransaction := trans
    busOut.tag := t
    busOut.index := ix
    busOut.cacheBlock := block
    busOut.state := ste
    busOut.valid := true.B
  }

  def fillBus(trans: UInt, oBundle: BusData, ste: UInt): Unit = {
    fillBus(
      trans,
      oBundle.tag,
      oBundle.index,
      L1Cache(oBundle.index),
      ste
    )
  }

  // DEBUG info
  def printStatus(i: UInt, status: UInt) = {
    switch(status) {
      is(Invalidated) {
        printf("pid %d: cacheStatus(%d) -> Invalidated\n", hostPid, i)
      }
      is(Shared) {
        printf("pid %d: cacheStatus(%d) -> Shared\n", hostPid, i)
      }
      is(Exclusive) {
        printf("pid %d: cacheStatus(%d) -> Exclusive\n", hostPid, i)
      }
      is(Owned) {
        printf("pid %d: cacheStatus(%d) -> Owned\n", hostPid, i)
      }
      is(Modified) {
        printf("pid %d: cacheStatus(%d) -> Modified\n", hostPid, i)
      }
    }
  }

  printf("pid %d: Stage 1\n", hostPid)

  // the request from the bus is from another processor and it hits a block
  when(guestId =/= hostPid && busValid && !io.busHold) {
    printf("pid %d: Stage 2\n", hostPid)
    when(isHit(busTag, busIndex)) {
      switch(cacheStatus(busIndex)) {
        is(Modified) { // dirty
          printf("pid %d: Stage 3\n", hostPid)
          when(busTrans === BusRd) {
            printf("pid %d: Stage 4\n", hostPid)
            // invalidated cache reading a modified cache
            cacheStatus(busIndex) := Owned
            printStatus(busIndex, Owned)
            fillBus(Flush, io.busIn, Modified)
          }.elsewhen(busTrans === BusRdX) {
            printf("pid %d: Stage 5\n", hostPid)
            // invalidated cache writing a modified cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            fillBus(Ack, io.busIn, Modified)
          }
        }
        is(Owned) { // dirty
          printf("pid %d: Stage 6\n", hostPid)
          // Owned cache does not respond to BusRd, since there are other Shared caches
          when(busTrans === BusRdX) {
            printf("pid %d: Stage 7\n", hostPid)
            // invalidated cache reading shared cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            fillBus(Ack, io.busIn, Owned)
          }.elsewhen(busTrans === BusUpgrade) {
            printf("pid %d: Stage 8\n", hostPid)
            // invalidated cache reading shared cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            fillBus(Ack, io.busIn, Owned)
          }
        }
        is(Exclusive) { // clean
          printf("pid %d: Stage 9\n", hostPid)
          when(busTrans === BusRd) {
            printf("pid %d: Stage 10\n", hostPid)
            cacheStatus(busIndex) := Owned
            printStatus(busIndex, Owned)
            fillBus(Flush, io.busIn, Exclusive)
          }.elsewhen(busTrans === BusRdX) {
            printf("pid %d: Stage 11\n", hostPid)
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            fillBus(Ack, io.busIn, Exclusive)
          }
        }
        is(Shared) { // clean
          printf("pid %d: Stage 12\n", hostPid)
          when(busTrans === BusRd) {
            printf("pid %d: Stage 13\n", hostPid)
            // response for a read request
            fillBus(Flush, io.busIn, Shared)
          }.elsewhen(busTrans === BusRdX) {
            printf("pid %d: Stage 14\n", hostPid)
            // invalidated cache reading shared cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            fillBus(Ack, io.busIn, Shared)
          }.elsewhen(busTrans === BusUpgrade) {
            printf("pid %d: Stage 15\n", hostPid)
            // shared cache writing shared cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            fillBus(Ack, io.busIn, Shared)
          }.elsewhen(busTrans === Flush) {
            // multiple shared caches give the same response
            // the bus chooses one of the shared caches, cancel this response
            when(busData === L1Cache(busIndex)) {
              printf("pid %d: Stage 16\n", hostPid)
              busOut := 0.U.asTypeOf(new BusData)
            }
          }
        }
      }
      // the request from the bus is from another processor but it does not hit a block
    }.elsewhen(prHlt && busAddr === prAddr) {
      printf("pid %d: Stage 17\n", hostPid)
      // the response is from another cache
      when(busTrans === Flush) {
        printf("pid %d: Stage 18\n", hostPid)
        prHlt := false.B
        disableIO()
        when(prOp === PrRd) {
          printf("pid %d: Stage 19\n", hostPid)
          L1Cache(busIndex) := busData
          printf("pid %d: L1Cache(%d) -> %d\n", hostPid, busIndex, busData)
          tagDirectory(index) := busTag
          cacheOutput := L1Cache(busIndex)
          cacheStatus(index) := Shared
          printStatus(busIndex, Shared)
        }
      }
    }
  }

  // processor requests
  printf("pid %d: prHlt: %d\n", hostPid, prHlt)
  when(guestId === hostPid && busValid && prHlt) {
    printf("pid %d: Stage 20\n", hostPid)
    validateBus := false.B
    busOut := 0.U.asTypeOf(new BusData)
    // the response is from the memory (one response can only respond to the origin of the request)
    when(busTrans === Fill && busAddr === prAddr) {
      prHlt := false.B
      disableIO()
      when(prOp === PrRd) {
        printf("pid %d: Stage 21\n", hostPid)
        L1Cache(busIndex) := busData
        printf("pid %d: L1Cache(%d) -> %d\n", hostPid, busIndex, busData)
        tagDirectory(index) := busTag
        cacheOutput := L1Cache(busIndex)
        cacheStatus(index) := Exclusive
        printStatus(busIndex, Exclusive)
      }
      // these transactions must wait until the bus receives ack to update this cache
    }.elsewhen(busTrans === BusUpgrade || busTrans === BusRdX) {
      when(!io.busAckHold) {
        printf("pid %d: Stage 23\n", hostPid)
        when(busTrans === BusUpgrade) {
          cacheStatus(index) := Exclusive
          printStatus(busIndex, Exclusive)
        }.otherwise {
          cacheStatus(index) := Modified
          printStatus(busIndex, Modified)
        }
        tagDirectory(index) := tag
        L1Cache(index) := busData
        printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, busData)
        prHlt := false.B
        disableIO()
      }
    }
  }.elsewhen(!prHlt) {
    printf("pid %d: Stage 24\n", hostPid)
    // the request from the processor hits
    when(isHit(tag, index)) {
      switch(cacheStatus(index)) {
        is(Modified) {
          switch(prOp) {
            is(PrRd) {
              printf("pid %d: Stage 25\n", hostPid)
              cacheOutput := L1Cache(index)
            }
            is(PrWr) {
              printf("pid %d: Stage 26\n", hostPid)
              L1Cache(index) := prData
              printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, prData)
              tagDirectory(index) := tag
            }
          }
        }
        is(Owned) {
          switch(prOp) {
            is(PrRd) {
              printf("pid %d: Stage 27\n", hostPid)
              cacheOutput := L1Cache(index)
            }
            is(PrWr) {
              printf("pid %d: Stage 28\n", hostPid)
              fillBus(BusUpgrade, tag, index, prData, Owned)
              validateBus := true.B

              prHlt := true.B
            }
          }
        }
        is(Exclusive) {
          switch(prOp) {
            is(PrRd) {
              printf("pid %d: Stage 29\n", hostPid)
              cacheOutput := L1Cache(index)
            }
            is(PrWr) {
              printf("pid %d: Stage 30\n", hostPid)
              L1Cache(index) := prData
              printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, prData)
              tagDirectory(index) := tag
              cacheStatus(index) := Modified
              printStatus(busIndex, Modified)
            }
          }
        }
        is(Shared) {
          switch(prOp) {
            is(PrRd) {
              printf("pid %d: Stage 31\n", hostPid)
              cacheOutput := L1Cache(index)
            }
            is(PrWr) {
              printf("pid %d: Stage 32\n", hostPid)
              fillBus(BusUpgrade, tag, index, prData, Shared)
              validateBus := true.B

              prHlt := true.B
            }
          }
        }
      }
      // the address does not hit
    }.otherwise {
      switch(prOp) {
        is(PrRd) {
          printf("pid %d: Stage 33\n", hostPid)
          fillBus(BusRd, tag, index, 0.U, Invalidated)
          validateBus := true.B

          prHlt := true.B
        }
        is(PrWr) {
          printf("pid %d: Stage 34\n", hostPid)
          fillBus(BusRdX, tag, index, prData, Invalidated)
          validateBus := true.B

          prHlt := true.B
        }
      }
    }
  }
}
