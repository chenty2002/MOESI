package MOESI_Diplomacy_Unified

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class L1Cache(val hostPid: UInt, val ps: MESIPS)(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new L1ModuleImp(this)
  val l1Node = new L1Node(Seq(ps))

  class L1ModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = l1Node.edges.out.head

    val busO = l1Node.out.map(_._1.masterOut.busData)
    val vBus = l1Node.out.map(_._1.masterOut.flag)

    val busI = l1Node.out.map(_._1.masterIn.busData).head
    val replFlag = l1Node.out.map(_._1.masterIn.flag).head
    val io = IO(new Bundle() {
      // processor operations
      val procOp = Input(UInt(procOpBits.W))
      val prAddr = Input(UInt(ep.addrBits.W))
      val prData = Input(UInt(ep.cacheBlockBits.W))

      // the data requests
      val cacheOutput = Output(UInt(ep.cacheBlockBits.W))
      val response = Output(new Bool)

      // Verify interface
      val cacheData = Output(Vec(cacheBlockNum, UInt(ep.cacheBlockBits.W)))
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

    val cacheData = RegInit(VecInit.fill(cacheBlockNum)(0.U(ep.cacheBlockBits.W)))
    val tagDirectory = RegInit(VecInit.fill(cacheBlockNum)(0.U(ep.tagBits.W)))
    val cacheStatus = RegInit(VecInit.fill(cacheBlockNum)(Invalidated))

    val (tag, index) = ep.parseAddr(prAddr)

    // bus requests
    val busId = busI.pid
    val busTag = busI.addrBundle.tag
    val busTrans = busI.busTransaction
    val busIndex = busI.addrBundle.index
    val busData = busI.addrBundle.cacheBlock
    val busState = busI.state
    val busValid = busI.valid
    val busAddr = busI.addrBundle.addr

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
      val (t, i) = ep.parseAddr(addr)
      cacheStatus(i) =/= Invalidated && tagDirectory(i) === t
    }

    def fillBus(trans: UInt, t: UInt, ix: UInt, ste: UInt): Unit = {
      fillBus(trans, t, ix, cacheData(ix), ste)
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
        cacheData(oBundle.addrBundle.index),
        ste
      )
    }

    // the request from the bus is from another processor and it hits a block
    when(busId =/= hostPid && busValid) {
      when(isHit(busAddr)) {
        switch(cacheStatus(busIndex)) {
          is(Modified) { // dirty
            when(busTrans === BusRd) {
              // invalidated cache reading a modified cache
              cacheStatus(busIndex) := Owned
              fillBus(Flush, busI, Modified)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              // invalidated cache writing a modified cache
              cacheStatus(busIndex) := Invalidated
            }
          }
          is(Owned) { // dirty
            when(busTrans === BusRd) {
              fillBus(Flush, busI, Owned)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              cacheStatus(busIndex) := Invalidated
              // the Repl trans from the bus appears when another only Shared cache will be replaced and this cache is Owned
              // so this cache needs to become Modified
            }.elsewhen(busTrans === Repl) {
              when(busState === Invalidated) {
                cacheStatus(busIndex) := Modified
              }.elsewhen(busState === Shared) {
                fillBus(Flush, busI, Owned)
              }
            }
          }
          is(Exclusive) { // clean
            when(busTrans === BusRd) {
              cacheStatus(busIndex) := Owned
              fillBus(Flush, busI, Exclusive)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              cacheStatus(busIndex) := Invalidated
            }
          }
          is(Shared) { // clean
            when(busTrans === BusRd) {
              // response for a read request
              fillBus(Flush, busI, Shared)
            }.elsewhen(busTrans === BusRdX || busTrans === BusUpgrade) {
              // invalidated cache reading shared cache
              cacheStatus(busIndex) := Invalidated
            }.elsewhen(busTrans === Repl) {
              fillBus(Flush, busI, Shared)
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
    when(replacing && replFlag) {
      replacing := false.B
      cacheData(replIndex) := replData
      tagDirectory(replIndex) := replTag
      prHlt := false.B
      io.response := true.B
      enableIO()
      ansBus := 0.U.asTypeOf(new BusData(ep))
      cacheOutput := replData
      cacheStatus(replIndex) := replState
    }

    when(busId === hostPid && busValid) {
      // the Repl trans from the bus appears when another Owned cache will be replaced and this cache is Shared
      // and this cache has been chosen to upgrade
      when(busTrans === Repl) {
        when(cacheStatus(busIndex) === Shared) {
          // when there are at least two Shared cache, this chosen cache becomes Owned
          when(busState === Owned) {
            cacheStatus(busIndex) := Owned
            // SPECIAL USE
            // when there is only one Shared cache, this chose cache becomes Modified
          }.otherwise {
            cacheStatus(busIndex) := Modified
          }
        }
          .elsewhen(prHlt) {
            // the response from the bus is from another processor but it does not hit a block
            when(busTrans === Flush && busAddr === prAddr) {
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
                  cacheData(busIndex) := busData
                  tagDirectory(index) := busTag
                  cacheOutput := busData
                  cacheStatus(index) := Shared
                  // otherwise it needs to inform other caches first
                }
                  .otherwise {
                    enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Shared)
                  }
              }
            }
              .otherwise {
                printf("pid %d: Stage 22\n", hostPid)
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
                      cacheData(busIndex) := busData
                      tagDirectory(index) := busTag
                      cacheOutput := busData
                      cacheStatus(index) := Exclusive
                      // otherwise it needs to inform other caches first
                    }.otherwise {
                      enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Exclusive)
                    }
                  }
                  // these transactions must wait until the bus receives ack to update this cache
                }.elsewhen(busTrans === BusUpgrade || busTrans === BusRdX) {
                  when(cacheStatus(busIndex) === Exclusive || cacheStatus(busIndex) === Invalidated || busTag === tagDirectory(index)) {
                    cacheStatus(index) := Modified
                    tagDirectory(index) := tag
                    cacheData(index) := busData
                    prHlt := false.B
                    respNext := true.B
                    enableIO()
                  }.otherwise {
                    when(busTrans === BusUpgrade) {
                      enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Exclusive)
                    }.otherwise {
                      enableRepl(tagDirectory(busIndex), busTag, busIndex, cacheData(busIndex), busData, Modified)
                    }
                  }
                }
              }
          }
      }
        .elsewhen(!prHlt) {
          // the request from the processor hits
          when(isHit(prAddr)) {
            // when the request on the bus invalidates the same address as the processor's,
            // it needs to delay the processor request
            when(invalidateNext && busI.addrBundle.addr === prAddr) {
              processing := true.B
              ioPrOp := ioPrOp
              ioPrAddr := ioPrAddr
              ioPrData := ioPrData
            }.otherwise {
              switch(cacheStatus(index)) {
                is(Modified) {
                  switch(prOp) {
                    is(PrRd) {
                      cacheOutput := cacheData(index)
                      io.response := true.B
                      enableIO()
                    }
                    is(PrWr) {
                      when(!answeringNext) {
                        cacheData(index) := prData
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
                      cacheOutput := cacheData(index)
                      io.response := true.B
                      enableIO()
                    }
                    is(PrWr) {
                      fillBus(BusUpgrade, tag, index, prData, Owned)
                      validateBus := true.B

                      prHlt := true.B
                    }
                  }
                }
                is(Exclusive) {
                  switch(prOp) {
                    is(PrRd) {
                      cacheOutput := cacheData(index)
                      io.response := true.B
                      enableIO()
                    }
                    is(PrWr) {
                      when(!answeringNext) {
                        cacheData(index) := prData
                        tagDirectory(index) := tag
                        cacheStatus(index) := Modified
                        io.response := true.B
                        enableIO()
                      }
                    }
                  }
                }
                is(Shared) {
                  switch(prOp) {
                    is(PrRd) {
                      cacheOutput := cacheData(index)
                      io.response := true.B
                      enableIO()
                    }
                    is(PrWr) {
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
                fillBus(BusRd, tag, index, 0.U, Invalidated)
                validateBus := true.B

                prHlt := true.B
              }
              is(PrWr) {
                fillBus(BusRdX, tag, index, prData, Invalidated)
                validateBus := true.B

                prHlt := true.B
              }
            }
          }
        }
    }
  }
}
