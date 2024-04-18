package MOESI_Diplomacy_Unified

import Util.RoundRobinArbiter
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class Bus(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new BusModuleImp(this)
  val busNode = new BusNode(
    dps => {
      require(dps.map(_.equals(dps.head)).reduce(_ || _))
      dps.head
    },
    ups => {
      require(ups.length == 1)
      ups.head
    }
  )

  class BusModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = busNode.edges.out.head

    val verify_io = IO(new Bundle() {
      val replacing = Output(new Bool)
    })

    val l1CachesIn = VecInit(busNode.in.map(_._1.masterOut.busData))
    val validateBus = VecInit(busNode.in.map(_._1.masterOut.flag))

    val memOut = busNode.out.map(_._1.masterOut.busData)
    val memW = busNode.out.map(_._1.masterOut.flag)

    val l1CachesOut = busNode.in.map(_._1.masterIn.busData)
    val replFlag = busNode.in.map(_._1.masterIn.flag)

    val memIn = busNode.out.map(_._1.masterIn.busData)

    val arbiter = Module(new RoundRobinArbiter(procNum))
    arbiter.io.requests := validateBus

    verify_io.replacing := l1CachesOut.head.valid && l1CachesOut.head.busTransaction === Repl

    val memHold = RegInit(false.B)
    val flushHold = RegInit(false.B)
    val busData = RegInit(0.U.asTypeOf(new BusData(ep)))
    val busDataBuffer = RegInit(0.U.asTypeOf(new BusData(ep)))
    val replDataBuffer = RegInit(0.U.asTypeOf(new BusData(ep)))
    val pid = RegInit(procNum.U(procNumBits.W))
    val processing = RegInit(false.B)
    val memData = RegInit(0.U.asTypeOf(new BusData(ep)))
    val memWen = RegInit(false.B)

    val memHoldFlag = RegInit(false.B)
    val flushCleanFlag = RegInit(false.B)
    replFlag.foreach(_ := false.B)

    def enableIO(): Unit = {
      processing := false.B
      busDataBuffer := 0.U.asTypeOf(new BusData(ep))
      busData := 0.U.asTypeOf(new BusData(ep))
    }

    // bus buffer, take the arbiter output only when the last transaction is done
    when(processing) {
      busData := busDataBuffer
    }
    memW.foreach(_ := memWen)
    memData := memIn.head

    when(memHold || flushHold) {
      // waiting for the response from the memory
      when(memHold && memData.valid && busData.valid && memData.addrBundle.addr === busData.addrBundle.addr) {
        when(memData.busTransaction === BusUpgrade || memData.busTransaction === Repl) {
          memHold := false.B
          memWen := false.B
          when(memData.busTransaction === Repl) {
            replFlag.foreach(_ := true.B)
          }
          when(!flushHold) {
            enableIO()
          }
        }.otherwise {
          // Fill transaction needs to wait one more beat for the cache to process
          when(memHoldFlag) {
            memHold := false.B
            memHoldFlag := false.B
            when(!flushHold) {
              enableIO()
            }
          }.otherwise {
            when(busData.valid && busData.busTransaction =/= Repl) {
              // copy the info of the memory (ignores pid because the response needs to correspond to the request)
              busData.busTransaction := memData.busTransaction
              busData.addrBundle.cacheBlock := memData.addrBundle.cacheBlock
              memHoldFlag := true.B
            }
          }
        }
      }
      // waiting for the response from other caches
      when(flushHold) {
        flushHold := false.B
        val flushFlag = l1CachesIn.map { l1 =>
          l1.valid &&
            l1.addrBundle.addr === busData.addrBundle.addr &&
            l1.busTransaction === Flush
        }
        when(!flushFlag.reduce(_ || _)) {
          when(busData.busTransaction =/= Repl) {
            memHold := true.B
          }
        }.otherwise {
          val countShared = PopCount(l1CachesIn.zip(flushFlag).map {
            case (l1In, flush) =>
              l1In.state === Shared && flush
          })
          // get the pid of the responded cache
          val tarPid = MuxCase(procNum.U(procNumBits.W),
            flushFlag.zipWithIndex.map {
              case (flushAndShared, i) =>
                flushAndShared -> i.U(procNumBits.W)
            })
          when(tarPid =/= procNum.U(procNumBits.W)) {
            when(busData.busTransaction === Repl) {
              // replacing Owned cache, choose a Shared cache to upgrade
              when(busData.state === Owned && l1CachesIn(tarPid).state === Shared) {
                busData.pid := tarPid
                // SPECIAL USE
                // when there is only one Shared cache, it needs to become Exclusive
                // otherwise, it only needs to become Owned
                when(countShared === 1.U) {
                  busData.state := Invalidated
                }
                flushCleanFlag := true.B
              }.elsewhen(countShared === 0.U) {
                // bus replace request, inform the Owned cache to become Modified
                // special usage
                busData.state := Invalidated
                flushCleanFlag := true.B
              }
            }.otherwise {
//              busData.pid := tarPid
              busData.busTransaction := Flush
              busData.addrBundle.cacheBlock := l1CachesIn(tarPid).addrBundle.cacheBlock
              //              busData := l1CachesIn(tarPid)
            }
          }
          // the flush data on the bus needs to be cleared, but it needs to last one more beat
          when(!memHold) {
            flushCleanFlag := true.B
          }
        }
      }
    }.otherwise {
      memWen := false.B
      when(flushCleanFlag) {
        when(busData.busTransaction === Repl) {
          replFlag.foreach(_ := true.B)
        }
        enableIO()
        flushCleanFlag := false.B
      }.otherwise {
        val hasRepl = l1CachesIn.map { l1 =>
          l1.valid && l1.busTransaction === Repl
        }
        when(busDataBuffer.busTransaction =/= Repl && hasRepl.reduce(_ || _)) {
          val replPid = MuxCase(procNum.U(procNumBits.W),
            hasRepl.zipWithIndex.map {
              case (replacing, i) =>
                replacing -> i.U(procNumBits.W)
            })
          busDataBuffer := l1CachesIn(replPid)
        }.elsewhen(busDataBuffer.valid) {
          processing := true.B
          busDataBuffer := busDataBuffer
        }.otherwise {
          busDataBuffer := l1CachesIn(OHToUInt(arbiter.io.grant))
        }
      }
      when(busData.valid) {
        when(busData.busTransaction === BusRd) {
          flushHold := true.B
        }.elsewhen(busData.busTransaction === BusUpgrade) {
          memHold := true.B
          memWen := true.B
        }.elsewhen(busData.busTransaction === BusRdX) {
          enableIO()
        }.elsewhen(busData.busTransaction === Repl) {
          switch(busData.state) {
            is(Modified) {
              // replacing Modified cache, only needs to write the data to memory
              memHold := true.B
              memWen := true.B
            }
            is(Owned) {
              // replacing Owned cache, there is at least one Shared cache,
              // choose one Shared cache to become Owned if there is at least two Shared cache,
              // choose the Shared cache to become Exclusive if there is only one Shared cache
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
    memOut.foreach(_ := busData)
    l1CachesOut.foreach(_ := busData)
  }
}