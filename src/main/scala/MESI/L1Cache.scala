package MESI

import chisel3._
import chisel3.util._

class L1Cache(val hostPid: UInt) extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    // processor oprations
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
    val busValid = Input(new Bool)
    // the cache needs to use the bus
    val validateBus = Output(new Bool)

    // direct access to the memory
    val mem = Input(Vec(1<<addrBits, UInt(cacheBlockBits.W)))
    val memAddr = Output(UInt(addrBits.W))
    val memWr = Output(UInt(cacheBlockBits.W))
    val memWen = Output(new Bool)
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

  io.memAddr := memAddr
  io.memWr := memWr
  io.memWen := memWen

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

  // whether the address hits
  def isHit(t: UInt, i: UInt): Bool = {
    cacheStatus(i) =/= Invalidated && tagDirectory(index) === t
  }

  // the request from the bus is from another processor and it hits a block
  when(guestId =/= hostPid && isHit(busTag, busIndex) && io.busValid) {
    switch(cacheStatus(busIndex)) {
      is(Modified) {
        switch(busTrans) {
          is(BusRd) {
            cacheStatus(busIndex) := Shared
            busOut.pid := hostPid
            busOut.busTransaction := Flush
            busOut.tag := busTag
            busOut.index := busIndex
            busOut.cacheBlock := L1Cache(busIndex)

            memWen := true.B
            memAddr := getAddr(index, tag)
            memWr := L1Cache(busIndex)
            validateBus := true.B
          }
          is(BusRdX) {
            cacheStatus(busIndex) := Invalidated
            busOut.pid := hostPid
            busOut.busTransaction := Flush
            busOut.tag := busTag
            busOut.index := busIndex
            busOut.cacheBlock := L1Cache(busIndex)

            validateBus := true.B
          }
        }
      }
      is(Exclusive) {
        switch(busTrans) {
          is(BusRd) {
            cacheStatus(busIndex) := Shared
          }
          is(BusRdX) {
            cacheStatus(busIndex) := Invalidated
          }
        }
      }
      is(Shared) {
        switch(busTrans) {
          is(BusRdX) {
            cacheStatus(busIndex) := Invalidated
          }
          is(BusUpgrade) {
            cacheStatus(busIndex) := Invalidated
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
                is(PrWr) {
                  L1Cache(busIndex) := io.cacheInput
                  tagDirectory(index) := busTag
                  cacheStatus(index) := Modified
                }
              }
            }
          }
        }
      }
    }
    // the request from the bus is from another processor but it does not hit a block
  }.elsewhen(guestId =/= hostPid && !isHit(busTag, busIndex) && io.busValid) {
    when(prHlt) {
      switch(busTrans) {
        is(Flush) {
          prHlt := false.B
          switch(io.procOp) {
            is(PrRd) {
              L1Cache(busIndex) := busData
              tagDirectory(index) := busTag
              cacheOutput := L1Cache(busIndex)
              cacheStatus(index) := Shared
            }
            is(PrWr) {
              L1Cache(busIndex) := io.cacheInput
              tagDirectory(index) := busTag
              cacheStatus(index) := Modified
            }
          }
        }
      }
    }
  }

  // processor requests
  val invalidStateCounter = RegInit(0.U((procNumBits+1).W))

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

            L1Cache(index) := io.cacheInput
            tagDirectory(index) := tag
            cacheStatus(index) := Modified
          }
        }
      }
      is(Invalidated) {
        switch(io.procOp) {
          is(PrRd) {
            busOut.pid := hostPid
            busOut.busTransaction := BusUpgrade
            busOut.tag := tag
            busOut.index := index

            // the cache needs to wait for the response from the bus
            validateBus := true.B
            prHlt := true.B
            when(invalidStateCounter === (procNum+1).U) {
              busOut.busTransaction := Fill
              busOut.cacheBlock := io.mem(io.prAddr)

              validateBus := true.B
              L1Cache(index) := io.mem(io.prAddr)
              tagDirectory(index) := tag
              cacheOutput := L1Cache(index)
              cacheStatus(index) := Exclusive
              prHlt := false.B
              invalidStateCounter := 0.U
            }.otherwise {
              invalidStateCounter := invalidStateCounter + 1.U
            }
          }
          is(PrWr) {
            busOut.pid := hostPid
            busOut.busTransaction := BusRdX
            busOut.tag := tag
            busOut.index := index

            // the cache needs to wait for the response from the bus
            validateBus := true.B
            prHlt := true.B
            when(invalidStateCounter === (procNum+1).U) {
              busOut.busTransaction := Fill
              busOut.cacheBlock := io.mem(io.prAddr)

              validateBus := true.B
              L1Cache(index) := io.mem(io.prAddr)
              tagDirectory(index) := tag
              L1Cache(index) := io.cacheInput
              cacheStatus(index) := Modified
              prHlt := false.B
              invalidStateCounter := 0.U
            }.otherwise {
              invalidStateCounter := invalidStateCounter + 1.U
            }
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

        validateBus := true.B
        prHlt := true.B
        when(invalidStateCounter === (procNum+1).U) {
          busOut.busTransaction := Fill
          busOut.cacheBlock := io.mem(io.prAddr)

          validateBus := true.B
          L1Cache(index) := io.mem(io.prAddr)
          tagDirectory(index) := tag
          cacheOutput := L1Cache(index)
          when(tag === busTag && index === busIndex) {
            cacheStatus(index) := Shared
          }.otherwise {
            cacheStatus(index) := Exclusive
          }
          prHlt := false.B
          invalidStateCounter := 0.U
        }.otherwise {
          invalidStateCounter := invalidStateCounter + 1.U
        }
      }
      is(PrWr) {
        busOut.pid := hostPid
        busOut.busTransaction := BusRdX
        busOut.tag := tag
        busOut.index := index

        validateBus := true.B
        prHlt := true.B
        when(invalidStateCounter === (procNum+1).U) {
          busOut.busTransaction := Fill
          busOut.cacheBlock := io.mem(io.prAddr)

          validateBus := true.B
          L1Cache(index) := io.mem(io.prAddr)
          tagDirectory(index) := tag
          L1Cache(index) := io.cacheInput
          cacheStatus(index) := Modified
          prHlt := false.B
          invalidStateCounter := 0.U
        }.otherwise {
          invalidStateCounter := invalidStateCounter + 1.U
        }
      }
    }
  }

  validateBus := false.B
}
