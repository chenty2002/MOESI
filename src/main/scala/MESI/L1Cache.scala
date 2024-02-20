package MESI

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters

class L1Cache(val hostPid: UInt) extends Module with HasMESIParameters {
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

    // whether the bus is valid to read
//    val busValid = Input(new Bool)
//    val busPid = Input(UInt(procNumBits.W))
    // the cache needs to use the bus
    val validateBus = Output(new Bool)
  })

  val prHlt = RegInit(false.B)
  val cacheOutput = RegInit(0.U(cacheBlockBits.W))
  val busOut = RegInit(0.U.asTypeOf(new BusData))
  val validateBus = RegInit(false.B)

  val memAddr = RegInit(0.U(addrBits.W))
  val memWr = RegInit(0.U(cacheBlockBits.W))
  val memWen = RegInit(false.B)

  io.prHlt := prHlt
  io.validateBus := validateBus
  io.busOut := busOut
  io.cacheOutput := cacheOutput

  val L1Cache = RegInit(VecInit.fill(cacheBlockNum)(0.U(cacheBlockBits.W)))
  val tagDirectory = RegInit(VecInit.fill(cacheBlockNum)(0.U(tagBits.W)))
  val cacheStatus = RegInit(VecInit.fill(cacheBlockNum)(Invalidated))

  val (index, tag) = parseAddr(io.prAddr)

  // bus requests
  val guestId = io.busIn.pid
  val busTag = io.busIn.tag
  val busTrans = io.busIn.busTransaction
  val busIndex = io.busIn.index
  val busData = io.busIn.cacheBlock
  val busState = io.busIn.state
  val busValid = io.busIn.valid && guestId =/= 0.U

  // whether the address hits
  def isHit(t: UInt, i: UInt): Bool = {
    cacheStatus(i) =/= Invalidated && tagDirectory(index) === t
  }

  when(guestId === hostPid && busValid) {
    validateBus := false.B
    busOut := 0.U.asTypeOf(new BusData)
    when(busTrans === BusUpgrade || busTrans === BusRdX) {
      cacheStatus(index) := Modified
      prHlt := false.B
    }
  }

  // the request from the bus is from another processor and it hits a block
  when(guestId =/= hostPid && isHit(busTag, busIndex) && busValid) {
    switch(cacheStatus(busIndex)) {
      is(Modified) { // dirty
        switch(busTrans) {
          is(BusRd) {
            // invalidated cache reading a modified cache
            cacheStatus(busIndex) := Shared
            busOut.pid := hostPid
            busOut.busTransaction := Flush
            busOut.tag := busTag
            busOut.index := busIndex
            busOut.cacheBlock := L1Cache(busIndex)
            busOut.state := Modified
            busOut.valid := true.B

            validateBus := true.B
          }
          is(BusRdX) {
            // invalidated cache writing a modified cache
            cacheStatus(busIndex) := Invalidated
            validateBus := false.B
          }
        }
      }
      is(Exclusive) { // clean
        switch(busTrans) {
          is(BusRd) {
            cacheStatus(busIndex) := Shared
            busOut.pid := hostPid
            busOut.busTransaction := Flush
            busOut.tag := busTag
            busOut.index := busIndex
            busOut.cacheBlock := L1Cache(busIndex)
            busOut.state := Exclusive
            busOut.valid := true.B

            validateBus := true.B
          }
          is(BusRdX) {
            cacheStatus(busIndex) := Invalidated
            validateBus := false.B
          }
        }
      }
      is(Shared) { // clean
        switch(busTrans) {
          is(BusRd) {
            // response for a read request
            busOut.pid := hostPid
            busOut.busTransaction := Flush
            busOut.tag := busTag
            busOut.index := busIndex
            busOut.cacheBlock := L1Cache(busIndex)
            busOut.state := Shared
            busOut.valid := true.B

            validateBus := true.B
          }
          is(BusRdX) {
            // invalidated cache reading shared cache
            cacheStatus(busIndex) := Invalidated
            validateBus := false.B
          }
          is(BusUpgrade) {
            // shared cache writing shared cache
            cacheStatus(busIndex) := Invalidated
            validateBus := false.B
          }
          is(Flush) {
            // multiple shared caches give the same response
            // the bus chooses another shared cache, cancel this response
            when(busData === L1Cache(busIndex)) {
              validateBus := false.B
            }
          }
        }
      }
      is(Invalidated) {
        when(prHlt) {
          switch(busTrans) {
            is(Flush) {
              prHlt := false.B
              switch(io.procOp) {
                is(PrRd) {
                  L1Cache(busIndex) := busData
                  cacheOutput := L1Cache(busIndex)
                  cacheStatus(index) := Shared
                }
              }
            }
            is(Fill) {
              prHlt := false.B
              switch(io.procOp) {
                is(PrRd) {
                  L1Cache(busIndex) := busData
                  cacheOutput := L1Cache(busIndex)
                  cacheStatus(index) := Exclusive
                }
              }
            }
          }
        }
      }
    }
    // the request from the bus is from another processor but it does not hit a block
  }
//    .elsewhen(guestId =/= hostPid && !isHit(busTag, busIndex) && busValid) {
//    when(prHlt) {
//      switch(busTrans) {
//        is(Flush) {
//          prHlt := false.B
//          switch(io.procOp) {
//            is(PrRd) {
//              L1Cache(busIndex) := busData
//              tagDirectory(index) := busTag
//              cacheOutput := L1Cache(busIndex)
//              cacheStatus(index) := Shared
//            }
//            is(PrWr) {
//              L1Cache(busIndex) := io.cacheInput
//              tagDirectory(index) := busTag
//              cacheStatus(index) := Modified
//            }
//          }
//        }
//      }
//    }
//  }

  // processor requests
//  val invalidStateCounter = RegInit(0.U((procNumBits+1).W))

  // the request from the processor hits
  when(isHit(tag, index)) {
    switch(cacheStatus(index)) {
      is(Modified) {
        switch(io.procOp) {
          is(PrRd) {
            cacheOutput := L1Cache(index)
          }
          is(PrWr) {
            L1Cache(index) := io.cacheInput
            tagDirectory(index) := tag
          }
        }
      }
      is(Exclusive) {
        switch(io.procOp) {
          is(PrRd) {
            cacheOutput := L1Cache(index)
          }
          is(PrWr) {
            L1Cache(index) := io.cacheInput
            tagDirectory(index) := tag
            cacheStatus(index) := Modified
          }
        }
      }
      is(Shared) {
        switch(io.procOp) {
          is(PrRd) {
            cacheOutput := L1Cache(index)
          }
          is(PrWr) {
            busOut.pid := hostPid
            busOut.busTransaction := BusUpgrade
            busOut.tag := tag
            busOut.index := index
            busOut.valid := true.B
            busOut.state := Shared
            validateBus := true.B

            prHlt := true.B
          }
        }
      }
      is(Invalidated) {
        switch(io.procOp) {
          is(PrRd) {
            busOut.pid := hostPid
            busOut.busTransaction := BusRd
            busOut.tag := tag
            busOut.index := index
            busOut.valid := true.B
            busOut.state := Invalidated
            validateBus := true.B

            prHlt := true.B
          }
          is(PrWr) {
            busOut.pid := hostPid
            busOut.busTransaction := BusRdX
            busOut.tag := tag
            busOut.index := index
            busOut.valid := true.B
            busOut.state := Invalidated
            validateBus := true.B

            prHlt := true.B
          }
        }
      }
    }
    // the address does not hit
  }.otherwise {
    switch(io.procOp) {
      is(PrRd) {
        busOut.pid := hostPid
        busOut.busTransaction := BusRd
        busOut.tag := tag
        busOut.index := index
        busOut.valid := true.B
        busOut.state := Invalidated
        validateBus := true.B

        prHlt := true.B
      }
      is(PrWr) {
        busOut.pid := hostPid
        busOut.busTransaction := BusRdX
        busOut.tag := tag
        busOut.index := index
        busOut.valid := true.B
        busOut.state := Invalidated
        validateBus := true.B

        prHlt := true.B
      }
    }
  }
}
