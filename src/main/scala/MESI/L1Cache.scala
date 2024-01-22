package MESI

import chisel3._
import chisel3.util._

class L1Cache(val hostPid: UInt) extends Module with HasMESIParameters {
  val io = IO(new Bundle() {
    val coreOp = Input(UInt(coreOpBits.W))
    val prHlt = Output(new Bool)
    val prAddr = Input(UInt(addrBits.W))

    val cacheOutput = Output(UInt(cacheBlockBits.W))
    val cacheInput = Input(UInt(cacheBlockBits.W))

    val busIn = Input(new BusData)
    val busOut = Output(new BusData)

    val busValid = Input(new Bool)
    val validateBus = Output(new Bool)

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

  // different access of bus for different cores
  val busAccessCounter = RegInit(0.U(coreNumBits.W))

  def isHit(t: UInt, i: UInt): Bool = {
    cacheStatus(i) =/= Invalidated && tagDirectory(index) === t
  }

  busAccessCounter := busAccessCounter + 1.U
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
              switch(io.coreOp) {
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
  }.elsewhen(guestId =/= hostPid && !isHit(busTag, busIndex) && io.busValid) {
    when(prHlt) {
      switch(busTrans) {
        is(Flush) {
          prHlt := false.B
          switch(io.coreOp) {
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

  // core requests
  val invalidStateCounter = RegInit(0.U((coreNumBits+1).W))

  when(isHit(tag, index)) {
    switch(cacheStatus(index)) {
      is(Modified) {
        switch(io.coreOp) {
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
        switch(io.coreOp) {
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
        switch(io.coreOp) {
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
        switch(io.coreOp) {
          is(PrRd) {
            busOut.pid := hostPid
            busOut.busTransaction := BusUpgrade
            busOut.tag := tag
            busOut.index := index

            validateBus := true.B
            prHlt := true.B
            when(invalidStateCounter === (coreNum+1).U) {
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

            validateBus := true.B
            prHlt := true.B
            when(invalidStateCounter === (coreNum+1).U) {
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
  }.otherwise {
    switch(io.coreOp) {
      is(PrRd) {
        busOut.pid := hostPid
        busOut.busTransaction := BusRd
        busOut.tag := tag
        busOut.index := index

        validateBus := true.B
        prHlt := true.B
        when(invalidStateCounter === (coreNum+1).U) {
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
        when(invalidStateCounter === (coreNum+1).U) {
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
