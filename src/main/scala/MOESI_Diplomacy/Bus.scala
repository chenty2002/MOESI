package MOESI_Diplomacy

import Util.RoundRobinArbiter
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class Bus(implicit p: Parameters) extends LazyModule with HasMOESIParameters {
  lazy val module = new BusModuleImp(this)
  val busDNode = Seq.fill(procNum)(new BusDNode(_.head, _.head))
  val busUNode = Seq.fill(procNum)(new BusUNode(_.head, _.head))

  class BusModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = busUNode.head.edges.out.head

    val verify_io = IO(new Bundle() {
      val replacing = Output(new Bool)
    })

    val l1CachesIn = VecInit(busDNode.map(_.in.head._1.busData))
    val validateBus = VecInit(busDNode.map(_.in.head._1.flag))

    val memOut = busDNode.map(_.out.head._1.busData)
    val memW = busDNode.map(_.out.head._1.flag)

    val l1CachesOut = busUNode.map(_.out.head._1.busData)
    val replFlag = busUNode.map(_.out.head._1.flag)

    val memIn = busUNode.map(_.in.head._1.busData)

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

    printf("bus: Stage 1\n")
    when(memHold || flushHold) {
      printf("bus: Stage 2\n")
      // waiting for the response from the memory
      when(memHold && memData.valid && busData.valid && memData.addrBundle.addr === busData.addrBundle.addr) {
        printf("bus: Stage 4\n")
        when(memData.busTransaction === BusUpgrade || memData.busTransaction === Repl) {
          printf("bus: Stage 5\n")
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
            printf("bus: Stage 6\n")
            memHold := false.B
            memHoldFlag := false.B
            when(!flushHold) {
              enableIO()
            }
          }.otherwise {
            printf("bus: Stage 7\n")
            // copy the info of the memory (ignores pid because the response needs to correspond to the request)
            busData.busTransaction := memData.busTransaction
            busData.addrBundle.cacheBlock := memData.addrBundle.cacheBlock
            memHoldFlag := true.B
          }
        }
      }
      // waiting for the response from other caches
      when(flushHold) {
        printf("bus: Stage 8\n")
        flushHold := false.B
        val flushFlag = l1CachesIn.map { l1 =>
          l1.valid &&
            l1.addrBundle.addr === busData.addrBundle.addr &&
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
          printf("bus: TarPid: %d\n", tarPid)
          when(tarPid =/= procNum.U(procNumBits.W)) {
            when(busData.busTransaction === Repl) {
              // replacing Owned cache, choose a Shared cache to upgrade
              when(busData.state === Owned && l1CachesIn(tarPid).state === Shared) {
                printf("bus: Stage 12\n")
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
              printf("bus: Stage 13\n")
              busData := l1CachesIn(tarPid)
            }
          }
          // the flush data on the bus needs to be cleared, but it needs to last one more beat
          when(!memHold) {
            flushCleanFlag := true.B
          }
        }
      }
    }.otherwise {
      printf("bus: Stage 14\n")
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
    printf("bus: Hold: %d\n", memHold || flushHold)
  }
}