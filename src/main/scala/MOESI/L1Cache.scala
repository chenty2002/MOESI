package MOESI

import chisel3._
import chisel3.util._

class L1Cache(val hostPid: UInt) extends Module with HasMOESIParameters {
  val io = IO(new Bundle() {
    // processor operations
    val procOp = Input(UInt(procOpBits.W))
    val prAddr = Input(UInt(addrBits.W))
    val prData = Input(UInt(cacheBlockBits.W))

    val cacheOutput = Output(UInt(cacheBlockBits.W))
    val response = Output(new Bool)

    // the requests on the bus
    val busIn = Input(new BusData)
    val busOut = Output(new BusData)
    val busReplFlag = Input(new Bool)
    // the cache needs to use the bus
    val validateBus = Output(new Bool)

    // Verify interface
    val cacheData = Output(Vec(cacheBlockNum, UInt(cacheBlockBits.W)))
    val cacheStatus = Output(Vec(cacheBlockNum, UInt(stateBits.W)))
    val tagDirectory = Output(Vec(cacheBlockNum, UInt(tagBits.W)))
  })
  val respNext = RegInit(false.B)
  val prHlt = RegInit(false.B)
  val processing = RegInit(false.B)
  val cacheOutput = WireDefault(0.U(cacheBlockBits.W))
  val busOut = RegInit(0.U.asTypeOf(new BusData))
  val validateBus = RegInit(false.B)
  val answering = WireDefault(false.B)
  val holdAns = RegInit(false.B)
  val ansBus = RegInit(0.U.asTypeOf(new BusData))

  val prOp = RegInit(0.U(procOpBits.W))
  val prAddr = RegInit(0.U(addrBits.W))
  val prData = RegInit(0.U(cacheBlockBits.W))

  val ioPrOp = RegInit(0.U(procOpBits.W))
  val ioPrAddr = RegInit(0.U(addrBits.W))
  val ioPrData = RegInit(0.U(cacheBlockBits.W))

  val replacing = RegInit(false.B)
  val replTag = RegInit(0.U(tagBits.W))
  val replIndex = RegInit(0.U(indexBits.W))
  val replData = RegInit(0.U(cacheBlockBits.W))
  val replState = RegInit(0.U(stateBits.W))

  // when the cache finishes the last request and the next request is valid, it confirms to process
  when(ioPrOp === PrWr || ioPrOp === PrRd) {
    processing := true.B
    ioPrOp := ioPrOp
    ioPrAddr := ioPrAddr
    ioPrData := ioPrData
  }.otherwise {
    ioPrOp := io.procOp
    ioPrAddr := io.prAddr
    ioPrData := io.prData
  }
  when(processing) {
    prOp := ioPrOp
    prAddr := ioPrAddr
    prData := ioPrData
  }

  io.response := false.B
  respNext := false.B
  when(respNext) {
    io.response := true.B
    respNext := false.B
  }
  io.validateBus := validateBus
  io.cacheOutput := cacheOutput
  answering := holdAns
  holdAns := false.B

  // when the cache receives the flush/ack request from the bus, it needs to replace its busData
  // otherwise if the processor's request is at the same time, the flush/ack request will be ignored
  when(answering) {
    io.busOut := ansBus
  }.otherwise {
    when(busOut.busTransaction === Flush || busOut.busTransaction === Repl) {
      busOut := 0.U.asTypeOf(new BusData)
      io.busOut := 0.U.asTypeOf(new BusData)
    }.otherwise {
      io.busOut := busOut
    }
  }

  val cacheData = RegInit(VecInit.fill(cacheBlockNum)(0.U(cacheBlockBits.W)))
  val tagDirectory = RegInit(VecInit.fill(cacheBlockNum)(0.U(tagBits.W)))
  val cacheStatus = RegInit(VecInit.fill(cacheBlockNum)(Invalidated))

  val (tag, index) = parseAddr(prAddr)

  // bus requests
  val busId = io.busIn.pid
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
    busId,
    busTag,
    busTrans,
    busIndex,
    busData,
    busState,
    busValid,
    busAddr)

  io.cacheData := cacheData
  io.cacheStatus := cacheStatus
  io.tagDirectory := tagDirectory

  def enableIO(): Unit = {
    processing := false.B
    ioPrOp := 0.U
    ioPrAddr := 0.U
    ioPrData := 0.U
    prOp := 0.U
    prAddr := 0.U
    prData := 0.U
  }

  def enableRepl(old_t: UInt, new_t: UInt, ix: UInt, old_data: UInt, new_data: UInt, ste: UInt): Unit = {
    replacing := true.B
    replTag := new_t
    replIndex := ix
    replData := new_data
    replState := ste
    fillBus(Repl, old_t, ix, old_data, cacheStatus(ix))
    validateBus := true.B
  }

  // whether the address hits
  def isHit(addr: UInt): Bool = {
    val (t, i) = parseAddr(addr)
    cacheStatus(i) =/= Invalidated && tagDirectory(i) === t
  }

  def fillBus(trans: UInt, t: UInt, ix: UInt, ste: UInt): Unit = {
    fillBus(trans, t, ix, cacheData(ix), ste)
  }

  def fillBus(trans: UInt, t: UInt, ix: UInt, block: UInt, ste: UInt): Unit = {
    busOut.pid := hostPid
    busOut.busTransaction := trans
    busOut.tag := t
    busOut.index := ix
    busOut.cacheBlock := block
    busOut.state := ste
    busOut.valid := true.B
    when(trans === Flush || trans === Repl) {
      answering := true.B
      holdAns := true.B
      ansBus.pid := hostPid
      ansBus.busTransaction := trans
      ansBus.tag := t
      ansBus.index := ix
      ansBus.cacheBlock := block
      ansBus.state := ste
      ansBus.valid := true.B
    }
  }

  def fillBus(trans: UInt, oBundle: BusData, ste: UInt): Unit = {
    fillBus(
      trans,
      oBundle.tag,
      oBundle.index,
      cacheData(oBundle.index),
      ste
    )
  }

  // DEBUG info
  def printStatus(i: UInt, status: UInt): Unit = {
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
  when(busId =/= hostPid && busValid) {
    printf("pid %d: Stage 2\n", hostPid)
    when(isHit(busAddr)) {
      switch(cacheStatus(busIndex)) {
        is(Modified) { // dirty
          printf("pid %d: Stage 3\n", hostPid)
          when(busTrans === BusRd) {
            printf("pid %d: Stage 4\n", hostPid)
            // invalidated cache reading a modified cache
            cacheStatus(busIndex) := Owned
            printStatus(busIndex, Owned)
            fillBus(Flush, io.busIn, Modified)
          }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
            printf("pid %d: Stage 5\n", hostPid)
            // invalidated cache writing a modified cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
          }
        }
        is(Owned) { // dirty
          printf("pid %d: Stage 6\n", hostPid)
          when(busTrans === BusRd) {
            fillBus(Flush, io.busIn, Owned)
          }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
            printf("pid %d: Stage 7\n", hostPid)
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
            // the Repl trans from the bus appears when another only Shared cache will be replaced and this cache is Owned
            // so this cache needs to become Modified
          }.elsewhen(busTrans === Repl) {
            when(busState === Invalidated) {
              printf("pid %d: Stage 8\n", hostPid)
              cacheStatus(busIndex) := Modified
              printStatus(busIndex, Modified)
            }.elsewhen(busState === Shared) {
              fillBus(Flush, io.busIn, Owned)
            }
          }
        }
        is(Exclusive) { // clean
          printf("pid %d: Stage 9\n", hostPid)
          when(busTrans === BusRd) {
            printf("pid %d: Stage 10\n", hostPid)
            cacheStatus(busIndex) := Owned
            printStatus(busIndex, Owned)
            fillBus(Flush, io.busIn, Exclusive)
          }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
            printf("pid %d: Stage 11\n", hostPid)
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
          }
        }
        is(Shared) {
          printf("pid %d: Stage 12\n", hostPid)
          when(busTrans === BusRd) {
            printf("pid %d: Stage 13\n", hostPid)
            // response for a read request
            fillBus(Flush, io.busIn, Shared)
          }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
            printf("pid %d: Stage 14\n", hostPid)
            // invalidated cache reading shared cache
            cacheStatus(busIndex) := Invalidated
            printStatus(busIndex, Invalidated)
          }.elsewhen(busTrans === Repl) {
            printf("pid %d: Stage 15\n", hostPid)
            fillBus(Flush, io.busIn, Shared)
          }
        }
      }
    }
  }

  // when this is true, the status of the addr corresponding to the bus will be invalidated the next beat
  val invalidateNext = busId =/= hostPid && busValid && isHit(busAddr) &&
    (cacheStatus(busIndex) =/= Invalidated && (busTrans === BusRdX || busTrans === BusUpgrade))

  // when this is true, the cache will be answering the bus the next beat
  val answeringNext = busId =/= hostPid && busValid && isHit(busAddr) &&
    ((cacheStatus(busIndex) =/= Invalidated && busTrans === BusRd) ||
      (cacheStatus(busIndex) === Owned && busTrans === Repl && busState === Shared) ||
      (cacheStatus(busIndex) === Shared && busTrans === Repl))

  // the replacing transaction has been informed to other caches, this cache can be replaced
  when(replacing && io.busReplFlag) {
    printf("pid %d: Stage 20\n", hostPid)
    replacing := false.B
    cacheData(replIndex) := replData
    printf("pid %d: L1Cache(%d) -> %d\n", hostPid, replIndex, replData)
    tagDirectory(replIndex) := replTag
    prHlt := false.B
    io.response := true.B
    enableIO()
    ansBus := 0.U.asTypeOf(new BusData)
    cacheOutput := replData
    cacheStatus(replIndex) := replState
    printStatus(replIndex, replState)
  }

  when(busId === hostPid && busValid) {
    // the Repl trans from the bus appears when another Owned cache will be replaced and this cache is Shared
    // and this cache has been chosen to upgrade
    when(busTrans === Repl) {
      printf("pid %d: Stage 21\n", hostPid)
      when(cacheStatus(busIndex) === Shared) {
        // when there are at least two Shared cache, this chosen cache becomes Owned
        when(busState === Owned) {
          cacheStatus(busIndex) := Owned
          printStatus(busIndex, Owned)
          // SPECIAL USE
          // when there is only one Shared cache, this chose cache becomes Modified
        }.otherwise {
          cacheStatus(busIndex) := Modified
          printStatus(busIndex, Modified)
        }
      }
    }
      .elsewhen(prHlt) {
        // the response from the bus is from another processor but it does not hit a block
        when(busTrans === Flush && busAddr === prAddr) {
          printf("pid %d: Stage 17\n", hostPid)
          when(prOp === PrRd) {
            // when this position has Exclusive data or Invalidated data, it can be replaced at once
            when(cacheStatus(busIndex) === Exclusive || cacheStatus(busIndex) === Invalidated || busTag === tagDirectory(index)) {
              // if this cache is not the origin of the flush transaction
              // but it coincidentally receives just the data it wants, it can cancel its request
              when(validateBus) {
                validateBus := false.B
              }
              prHlt := false.B
              io.response := true.B
              enableIO()
              printf("pid %d: Stage 18\n", hostPid)
              cacheData(busIndex) := busData
              printf("pid %d: L1Cache(%d) -> %d\n", hostPid, busIndex, busData)
              tagDirectory(index) := busTag
              cacheOutput := busData
              cacheStatus(index) := Shared
              printStatus(index, Shared)
              // otherwise it needs to inform other caches first
            }
              .otherwise {
                printf("pid %d: Stage 19\n", hostPid)
                enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Shared)
              }
          }
        }
          .otherwise {
            printf("pid %d: Stage 22\n", hostPid)
            validateBus := false.B
            busOut := 0.U.asTypeOf(new BusData)
            // the response is from the memory (one response can only respond to the origin of the request)
            when(busTrans === Fill && busAddr === prAddr) {
              when(prOp === PrRd) {
                // when this position has Exclusive data or Invalidated data, it can be replaced at once
                when(cacheStatus(busIndex) === Exclusive || cacheStatus(busIndex) === Invalidated || busTag === tagDirectory(index)) {
                  prHlt := false.B
                  io.response := true.B
                  enableIO()
                  printf("pid %d: Stage 23\n", hostPid)
                  cacheData(busIndex) := busData
                  printf("pid %d: L1Cache(%d) -> %d\n", hostPid, busIndex, busData)
                  tagDirectory(index) := busTag
                  cacheOutput := busData
                  cacheStatus(index) := Exclusive
                  printStatus(index, Exclusive)
                  // otherwise it needs to inform other caches first
                }
                  .otherwise {
                    printf("pid %d: Stage 24\n", hostPid)
                    enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Exclusive)
                  }
              }
              // these transactions must wait until the bus receives ack to update this cache
            }
              .elsewhen(busTrans === BusUpgrade || busTrans === BusRdX) {
                when(cacheStatus(busIndex) === Exclusive || cacheStatus(busIndex) === Invalidated || busTag === tagDirectory(index)) {
                  printf("pid %d: Stage 25\n", hostPid)
                  when(busTrans === BusUpgrade) {
                    cacheStatus(index) := Exclusive
                    printStatus(index, Exclusive)
                  }
                    .otherwise {
                      cacheStatus(index) := Modified
                      printStatus(index, Modified)
                    }
                  tagDirectory(index) := tag
                  cacheData(index) := busData
                  printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, busData)
                  prHlt := false.B
                  respNext := true.B
                  enableIO()
                }
                  .otherwise {
                    printf("pid %d: Stage 26\n", hostPid)
                    when(busTrans === BusUpgrade) {
                      enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Exclusive)
                    }
                      .otherwise {
                        enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Modified)
                      }
                  }
              }
          }
      }
  }
    .elsewhen(!prHlt) {
      printf("pid %d: Stage 27\n", hostPid)
      // the request from the processor hits
      when(isHit(prAddr)) {
        // when the request on the bus invalidates the same address as the processor's,
        // it needs to delay the processor request
        when(invalidateNext && io.busIn.addr === prAddr) {
          processing := true.B
          ioPrOp := ioPrOp
          ioPrAddr := ioPrAddr
          ioPrData := ioPrData
        }
          .otherwise {
            switch(cacheStatus(index)) {
              is(Modified) {
                switch(prOp) {
                  is(PrRd) {
                    printf("pid %d: Stage 28\n", hostPid)
                    cacheOutput := cacheData(index)
                    io.response := true.B
                    enableIO()
                  }
                  is(PrWr) {
                    when(!answeringNext) {
                      printf("pid %d: Stage 29\n", hostPid)
                      cacheData(index) := prData
                      printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, prData)
                      tagDirectory(index) := tag
                      io.response := true.B
                      enableIO()
                    }
                  }
                }
              }
              is(Owned) {
                switch(prOp) {
                  is(PrRd) {
                    printf("pid %d: Stage 30\n", hostPid)
                    cacheOutput := cacheData(index)
                    io.response := true.B
                    enableIO()
                  }
                  is(PrWr) {
                    printf("pid %d: Stage 31\n", hostPid)
                    fillBus(BusUpgrade, tag, index, prData, Owned)
                    validateBus := true.B

                    prHlt := true.B
                  }
                }
              }
              is(Exclusive) {
                switch(prOp) {
                  is(PrRd) {
                    printf("pid %d: Stage 32\n", hostPid)
                    cacheOutput := cacheData(index)
                    io.response := true.B
                    enableIO()
                  }
                  is(PrWr) {
                    when(!answeringNext) {
                      printf("pid %d: Stage 33\n", hostPid)
                      cacheData(index) := prData
                      printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, prData)
                      tagDirectory(index) := tag
                      cacheStatus(index) := Modified
                      printStatus(index, Modified)
                      io.response := true.B
                      enableIO()
                    }
                  }
                }
              }
              is(Shared) {
                switch(prOp) {
                  is(PrRd) {
                    printf("pid %d: Stage 34\n", hostPid)
                    cacheOutput := cacheData(index)
                    io.response := true.B
                    enableIO()
                  }
                  is(PrWr) {
                    printf("pid %d: Stage 35\n", hostPid)
                    fillBus(BusUpgrade, tag, index, prData, Shared)
                    validateBus := true.B

                    prHlt := true.B
                  }
                }
              }
            }
          }
        // the address does not hit
      }
        .otherwise {
          switch(prOp) {
            is(PrRd) {
              printf("pid %d: Stage 36\n", hostPid)
              fillBus(BusRd, tag, index, 0.U, Invalidated)
              validateBus := true.B

              prHlt := true.B
            }
            is(PrWr) {
              printf("pid %d: Stage 37\n", hostPid)
              fillBus(BusRdX, tag, index, prData, Invalidated)
              validateBus := true.B

              prHlt := true.B
            }
          }
        }
    }
}
