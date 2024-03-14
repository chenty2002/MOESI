package MESI_Diplomacy

import Util.RoundRobinArbiter
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class Bus(implicit p: Parameters) extends LazyModule with HasMESIParameters {
  lazy val module = new BusModuleImp(this)
  val busDNode = Seq.fill(procNum)(new BusDNode(_.head, _.head))
  val busUNode = Seq.fill(procNum)(new BusUNode(_.head, _.head))

  class BusModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val ep = busUNode.head.edges.out.head

    val l1CachesIn = VecInit(busDNode.map(_.in.head._1.busData))
    val validateBus = VecInit(busDNode.map(_.in.head._1.flag))

    val memOut = busDNode.map(_.out.head._1.busData)
    val memW = busDNode.map(_.out.head._1.flag)

    val l1CachesOut = busUNode.map(_.out.head._1.busData)
    val hold = busUNode.map(_.out.head._1.flag)

    val memIn = busUNode.map(_.in.head._1.busData)

    val arbiter = Module(new RoundRobinArbiter(procNum))
    arbiter.io.requests := validateBus

    val valid = validateBus.reduce(_ || _)

    val memHold = RegInit(false.B)
    val flushHold = RegInit(false.B)
    val busData = RegInit(0.U.asTypeOf(new BusData(ep)))
    val pid = OHToUInt(arbiter.io.grant)
    val memData = RegInit(0.U.asTypeOf(new BusData(ep)))
    val memWen = RegInit(false.B)

    val memHoldFlag = RegInit(false.B)
    hold.foreach(_ := memHold || flushHold)

    //  pid := io.pidIn
    //  io.pidOut := pid
    memW.foreach(_ := memWen)
    memData := memIn.head

    printf("bus: Stage 1\n")
    when(memHold || flushHold) {
      printf("bus: Stage 2\n")
      when(memHold &&
        memData.valid &&
        busData.valid &&
        memData.addrBundle.addr === busData.addrBundle.addr) {
        printf("bus: Stage 3\n")
        when(memData.busTransaction === BusUpgrade) {
          memHold := false.B
          memWen := false.B
        }.otherwise {
          when(memHoldFlag) {
            busData := memData
            memHold := false.B
            memHoldFlag := false.B
          }.otherwise {
            memHoldFlag := true.B
          }
        }
      }
      when(flushHold) {
        printf("bus: Stage 4\n")
        flushHold := false.B
        val flushFlag = l1CachesIn.map { l1 =>
          l1.valid &&
            l1.addrBundle.addr === busData.addrBundle.addr &&
            l1.busTransaction === Flush
        }
        printf("bus: flushFlag (%d, %d, %d, %d)\n",
          flushFlag(0), flushFlag(1), flushFlag(2), flushFlag(3))
        when(!flushFlag.reduce(_ || _)) {
          printf("bus: Stage 5\n")
          memHold := true.B
        }.otherwise {
          printf("bus: Stage 6\n")
          val tarPid = MuxCase(procNum.U(procNumBits.W),
            flushFlag.zipWithIndex.map { flush =>
              flush._1 -> flush._2.U(procNumBits.W)
            })
          printf("bus: TarPid: %d\n", tarPid)
          when(tarPid =/= procNum.U(procNumBits.W)) {
            printf("bus: Stage 7\n")
            busData := l1CachesIn(tarPid)
            when(l1CachesIn(tarPid).state === Modified) {
              memWen := true.B
            }
          }
        }
      }
    }.otherwise {
      memWen := false.B
      busData := l1CachesIn(pid)
      when(valid) {
        printf("bus: Stage 8\n")

        when(busData.busTransaction === BusRd) {
          printf("bus: Stage 9\n")
          flushHold := true.B
        }.elsewhen(busData.busTransaction === BusUpgrade) {
          printf("bus: Stage 10\n")
          memHold := true.B
          memWen := true.B
        }
      }.otherwise {
        printf("bus: Stage 11\n")
        busData := 0.U.asTypeOf(new BusData(ep))
      }
    }
    memOut.foreach(_ := busData)
    l1CachesOut.foreach(_ := busData)
    printf("bus: Hold: %d\n", memHold || flushHold)
  }
}
