package MOESI_Diplomacy

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class L1Cache(val hostPid: UInt, val ps: MESIPS)(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new L1ModuleImp(this)
  val l1DNode = new L1SourceNode(Seq(ps))
  val l1UNode = new L1SinkNode(Seq(ps))

  class L1ModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = l1DNode.edges.out.head

    val busO = l1DNode.out.map(_._1.busData)
    val vBus = l1DNode.out.map(_._1.flag)

    val busI = l1UNode.in.map(_._1.busData).head
    val replFlag = l1UNode.in.map(_._1.flag).head
    val io = IO(new Bundle() {
      // processor operations
      val procOp = Input(UInt(procOpBits.W))
      val prAddr = Input(UInt(ep.addrBits.W))
      val prData = Input(UInt(ep.cacheBlockBits.W))

      // the data requests
      val cacheOutput = Output(UInt(ep.cacheBlockBits.W))
      val response = Output(new Bool)

      // DEBUG info
      val cacheStatus = Output(Vec(cacheBlockNum, UInt(stateBits.W)))
      val tagDirectory = Output(Vec(cacheBlockNum, UInt(ep.tagBits.W)))
    })

    val respNext = RegInit(false.B)
    val prHlt = RegInit(false.B)
    val processing = RegInit(false.B)
    val cacheOutput = WireDefault(0.U(ep.cacheBlockBits.W))
    val busOut = RegInit(0.U.asTypeOf(new BusData(ep)))
    val validateBus = RegInit(false.B)
    val answering = WireDefault(false.B)
    val holdAns = RegInit(false.B)
    val ansBus = RegInit(0.U.asTypeOf(new BusData(ep)))

    val prOp = RegInit(0.U(procOpBits.W))
    val prAddr = RegInit(0.U(ep.addrBits.W))
    val prData = RegInit(0.U(ep.cacheBlockBits.W))

    val ioPrOp = RegInit(0.U(procOpBits.W))
    val ioPrAddr = RegInit(0.U(ep.addrBits.W))
    val ioPrData = RegInit(0.U(ep.cacheBlockBits.W))

    val replacing = RegInit(false.B)
    val replTag = RegInit(0.U(ep.tagBits.W))
    val replIndex = RegInit(0.U(ep.indexBits.W))
    val replData = RegInit(0.U(ep.cacheBlockBits.W))
    val replacingState = RegInit(0.U(stateBits.W))

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
    vBus.foreach(_ := validateBus)
    io.cacheOutput := cacheOutput
    answering := holdAns
    holdAns := false.B

    // when the cache receives the flush/ack request from the bus, it needs to replace its busData
    // otherwise if the processor's request is at the same time, the flush/ack request will be ignored
    when(answering) {
      busO.foreach(_ := ansBus)
    }.otherwise {
      when(busOut.busTransaction === Flush || busOut.busTransaction === Repl) {
        busOut := 0.U.asTypeOf(new BusData(ep))
        busO.foreach(_ := 0.U.asTypeOf(new BusData(ep)))
      }.otherwise {
        busO.foreach(_ := busOut)
      }
    }

    val L1Cache = RegInit(VecInit.fill(cacheBlockNum)(0.U(ep.cacheBlockBits.W)))
    val tagDirectory = RegInit(VecInit.fill(cacheBlockNum)(0.U(ep.tagBits.W)))
    val cacheStatus = RegInit(VecInit.fill(cacheBlockNum)(Invalidated))

    val (tag, index) = ep.parseAddr(prAddr)

    // bus requests
    val guestId = busI.pid
    val busTag = busI.addrBundle.tag
    val busTrans = busI.busTransaction
    val busIndex = busI.addrBundle.index
    val busData = busI.addrBundle.cacheBlock
    val busState = busI.state
    val busValid = busI.valid
    val busAddr = busI.addrBundle.addr

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

    io.cacheStatus := cacheStatus
    io.tagDirectory := tagDirectory

    def enableIO(): Unit = {
      //    resp := true.B
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
      replacingState := ste
      fillBus(Repl, old_t, ix, old_data, cacheStatus(ix))
      validateBus := true.B
    }

    // whether the address hits
    def isHit(addr: UInt): Bool = {
      val (t, i) = ep.parseAddr(addr)
      cacheStatus(i) =/= Invalidated && tagDirectory(i) === t
    }

    def fillBus(trans: UInt, t: UInt, ix: UInt, ste: UInt): Unit = {
      fillBus(trans, t, ix, L1Cache(ix), ste)
    }

    def fillBus(trans: UInt, t: UInt, ix: UInt, block: UInt, ste: UInt): Unit = {
      busOut.pid := hostPid
      busOut.busTransaction := trans
      busOut.addrBundle.tag := t
      busOut.addrBundle.index := ix
      busOut.addrBundle.cacheBlock := block
      busOut.state := ste
      busOut.valid := true.B
      when(trans === Flush || trans === Repl) {
        answering := true.B
        holdAns := true.B
        ansBus.pid := hostPid
        ansBus.busTransaction := trans
        ansBus.addrBundle.tag := t
        ansBus.addrBundle.index := ix
        ansBus.addrBundle.cacheBlock := block
        ansBus.state := ste
        ansBus.valid := true.B
      }
    }

    def fillBus(trans: UInt, oBundle: BusData, ste: UInt): Unit = {
      fillBus(
        trans,
        oBundle.addrBundle.tag,
        oBundle.addrBundle.index,
        L1Cache(oBundle.addrBundle.index),
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
    when(guestId =/= hostPid && busValid) {
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
              fillBus(Flush, busI, Modified)
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
              fillBus(Flush, busI, Owned)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              printf("pid %d: Stage 7\n", hostPid)
              cacheStatus(busIndex) := Invalidated
              printStatus(busIndex, Invalidated)
              // the Repl trans from the bus appears when another only Shared cache will be replaced and this cache is Owned
              // so this cache needs to become Modified
            }.elsewhen(busTrans === Repl) {
              when(busState === Invalidated) {
                printf("pid %d: Stage 9\n", hostPid)
                cacheStatus(busIndex) := Modified
                printStatus(busIndex, Modified)
              }.elsewhen(busState === Shared) {
                fillBus(Flush, busI, Owned)
              }
            }
          }
          is(Exclusive) { // clean
            printf("pid %d: Stage 10\n", hostPid)
            when(busTrans === BusRd) {
              printf("pid %d: Stage 11\n", hostPid)
              cacheStatus(busIndex) := Owned
              printStatus(busIndex, Owned)
              fillBus(Flush, busI, Exclusive)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              printf("pid %d: Stage 12\n", hostPid)
              cacheStatus(busIndex) := Invalidated
              printStatus(busIndex, Invalidated)
            }
          }
          is(Shared) { // clean
            printf("pid %d: Stage 13\n", hostPid)
            when(busTrans === BusRd) {
              printf("pid %d: Stage 14\n", hostPid)
              // response for a read request
              fillBus(Flush, busI, Shared)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              printf("pid %d: Stage 15\n", hostPid)
              // invalidated cache reading shared cache
              cacheStatus(busIndex) := Invalidated
              printStatus(busIndex, Invalidated)
            }.elsewhen(busTrans === Repl) {
              printf("pid %d: Stage 18\n", hostPid)
              fillBus(Flush, busI, Shared)
            }
          }
        }
        // the request from the bus is from another processor but it does not hit a block
      }.elsewhen(prHlt && busAddr === prAddr) {
        printf("pid %d: Stage 18\n", hostPid)
        // the response is from another cache
        when(busTrans === Flush) {
          printf("pid %d: Stage 19\n", hostPid)
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
              printf("pid %d: Stage 20\n", hostPid)
              L1Cache(busIndex) := busData
              printf("pid %d: L1Cache(%d) -> %d\n", hostPid, busIndex, busData)
              tagDirectory(index) := busTag
              cacheOutput := busData
              cacheStatus(index) := Shared
              printStatus(index, Shared)
              // otherwise it needs to inform other caches first
            }.otherwise {
              printf("pid %d: Stage 21\n", hostPid)
              enableRepl(tagDirectory(busIndex), busTag, busIndex, L1Cache(busIndex), busData, Shared)
            }
          }
        }
      }
    }

    // the replacing transaction has been informed to other caches, this cache can be replaced
    when(replacing && replFlag) {
      printf("pid %d: Stage 22\n", hostPid)
      replacing := false.B
      L1Cache(replIndex) := replData
      printf("pid %d: L1Cache(%d) -> %d\n", hostPid, replIndex, replData)
      tagDirectory(replIndex) := replTag
      prHlt := false.B
      io.response := true.B
      enableIO()
      cacheOutput := replData
      cacheStatus(replIndex) := replacingState
      printStatus(replIndex, replacingState)
    }

    // the Repl trans from the bus appears when another Owned cache will be replaced and this cache is Shared
    // and this cache has been chosen to upgrade
    when(guestId === hostPid && busValid && busTrans === Repl) {
      printf("pid %d: Stage 28\n", hostPid)
      when(cacheStatus(busIndex) === Shared) {
        // when there are at least two Shared cache, this chosen cache becomes Owned
        when(busState === Owned) {
          cacheStatus(busIndex) := Owned
          printStatus(busIndex, Owned)
          // SPECIAL USE
          // when there is only one Shared cache, this chose cache becomes Exclusive
        }.otherwise {
          cacheStatus(busIndex) := Exclusive
          printStatus(busIndex, Exclusive)
        }
      }
    }

    // processor requests
    printf("pid %d: prHlt: %d\n", hostPid, prHlt)
    when(guestId === hostPid && busValid && prHlt) {
      printf("pid %d: Stage 23\n", hostPid)
      validateBus := false.B
      busOut := 0.U.asTypeOf(new BusData(ep))
      // the response is from the memory (one response can only respond to the origin of the request)
      when(busTrans === Fill && busAddr === prAddr) {
        when(prOp === PrRd) {
          // when this position has Exclusive data or Invalidated data, it can be replaced at once
          when(cacheStatus(busIndex) === Exclusive || cacheStatus(busIndex) === Invalidated || busTag === tagDirectory(index)) {
            prHlt := false.B
            io.response := true.B
            enableIO()
            printf("pid %d: Stage 25\n", hostPid)
            L1Cache(busIndex) := busData
            printf("pid %d: L1Cache(%d) -> %d\n", hostPid, busIndex, busData)
            tagDirectory(index) := busTag
            cacheOutput := busData
            cacheStatus(index) := Exclusive
            printStatus(index, Exclusive)
            // otherwise it needs to inform other caches first
          }.otherwise {
            printf("pid %d: Stage 26\n", hostPid)
            enableRepl(tagDirectory(busIndex), busTag, busIndex, L1Cache(busIndex), busData, Exclusive)
          }
        }
        // these transactions must wait until the bus receives ack to update this cache
      }.elsewhen(busTrans === BusUpgrade || busTrans === BusRdX) {
        when(cacheStatus(busIndex) === Exclusive || cacheStatus(busIndex) === Invalidated || busTag === tagDirectory(index)) {
          printf("pid %d: Stage 27\n", hostPid)
          when(busTrans === BusUpgrade) {
            cacheStatus(index) := Exclusive
            printStatus(index, Exclusive)
          }.otherwise {
            cacheStatus(index) := Modified
            printStatus(index, Modified)
          }
          tagDirectory(index) := tag
          L1Cache(index) := busData
          printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, busData)
          prHlt := false.B
          respNext := true.B
          enableIO()
        }.otherwise {
          printf("pid %d: Stage 26\n", hostPid)
          when(busTrans === BusUpgrade) {
            enableRepl(tagDirectory(busIndex), busTag, busIndex, L1Cache(busIndex), busData, Exclusive)
          }.otherwise {
            enableRepl(tagDirectory(busIndex), busTag, busIndex, L1Cache(busIndex), busData, Modified)
          }
        }
      }
    }.elsewhen(!prHlt) {
      printf("pid %d: Stage 29\n", hostPid)
      // the request from the processor hits
      when(isHit(prAddr)) {
        when(guestId =/= hostPid && busValid && isHit(busAddr) &&
          (cacheStatus(busIndex) =/= Invalidated && (busTrans === BusRdX || busTrans === BusUpgrade)) &&
          busI.addrBundle.addr === prAddr) {
          processing := true.B
          ioPrOp := ioPrOp
          ioPrAddr := ioPrAddr
          ioPrData := ioPrData
        }.otherwise {
          switch(cacheStatus(index)) {
            is(Modified) {
              switch(prOp) {
                is(PrRd) {
                  printf("pid %d: Stage 30\n", hostPid)
                  cacheOutput := L1Cache(index)
                  io.response := true.B
                  enableIO()
                }
                is(PrWr) {
                  printf("pid %d: Stage 31\n", hostPid)
                  L1Cache(index) := prData
                  printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, prData)
                  tagDirectory(index) := tag
                  io.response := true.B
                  enableIO()
                }
              }
            }
            is(Owned) {
              switch(prOp) {
                is(PrRd) {
                  printf("pid %d: Stage 32\n", hostPid)
                  cacheOutput := L1Cache(index)
                  io.response := true.B
                  enableIO()
                }
                is(PrWr) {
                  printf("pid %d: Stage 33\n", hostPid)
                  fillBus(BusUpgrade, tag, index, prData, Owned)
                  validateBus := true.B

                  prHlt := true.B
                }
              }
            }
            is(Exclusive) {
              switch(prOp) {
                is(PrRd) {
                  printf("pid %d: Stage 34\n", hostPid)
                  cacheOutput := L1Cache(index)
                  io.response := true.B
                  enableIO()
                }
                is(PrWr) {
                  printf("pid %d: Stage 35\n", hostPid)
                  L1Cache(index) := prData
                  printf("pid %d: L1Cache(%d) -> %d\n", hostPid, index, prData)
                  tagDirectory(index) := tag
                  cacheStatus(index) := Modified
                  printStatus(index, Modified)
                  io.response := true.B
                  enableIO()
                }
              }
            }
            is(Shared) {
              switch(prOp) {
                is(PrRd) {
                  printf("pid %d: Stage 36\n", hostPid)
                  cacheOutput := L1Cache(index)
                  io.response := true.B
                  enableIO()
                }
                is(PrWr) {
                  printf("pid %d: Stage 37\n", hostPid)
                  fillBus(BusUpgrade, tag, index, prData, Shared)
                  validateBus := true.B

                  prHlt := true.B
                }
              }
            }
          }
        }
        // the address does not hit
      }.otherwise {
        switch(prOp) {
          is(PrRd) {
            printf("pid %d: Stage 38\n", hostPid)
            fillBus(BusRd, tag, index, 0.U, Invalidated)
            validateBus := true.B

            prHlt := true.B
          }
          is(PrWr) {
            printf("pid %d: Stage 39\n", hostPid)
            fillBus(BusRdX, tag, index, prData, Invalidated)
            validateBus := true.B

            prHlt := true.B
          }
        }
      }
    }
  }
}
