package MOESI

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class PeekTest extends AnyFlatSpec with ChiselScalatestTester with HasMOESIParameters {
  def pokeProc(moesi: MOESITop, procOp: Seq[Int], addr: Seq[Int], data: Seq[Int] = Seq.fill(4)(0)): Unit = {
    for (i <- 0 until procNum) {
      if (procOp(i) != 0) {
        moesi.io.procOp(i).poke(procOp(i).U(procOpBits.W))
        moesi.io.addr(i).poke(addr(i).U(addrBits.W))
        moesi.io.prData(i).poke(data(i).U(cacheBlockBits.W))
      } else {
        moesi.io.procOp(i).poke(0.U(procOpBits.W))
        moesi.io.addr(i).poke(0.U(addrBits.W))
        moesi.io.prData(i).poke(0.U(cacheBlockBits.W))
      }
    }
  }

  def endSign(moesi: MOESITop): Unit = {
    pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0,0,0,0))
    moesi.clock.step()
  }
  
  "Final" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      do {
        pokeProc(moesi, Seq(1, 0, 0, 0), Seq(0, 0, 0, 0))
        moesi.clock.step()
        result = moesi.io.cacheOutput(0).peekInt()
      } while (!moesi.io.procResp(0).peekBoolean())
      println("result: ", result)

      do {
        pokeProc(moesi, Seq(1, 0, 0, 0), Seq(0, 0, 0, 0))
        moesi.clock.step()
        result = moesi.io.cacheOutput(0).peekInt()
      } while (!moesi.io.procResp(0).peekBoolean())
      println("result: ", result)

      do {
        pokeProc(moesi, Seq(2, 0, 0, 0), Seq(0, 0, 0, 0), Seq(1, 0, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(0).peekBoolean())

      do {
        pokeProc(moesi, Seq(2, 0, 0, 0), Seq(0, 0, 0, 0), Seq(2, 0, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(0).peekBoolean())

      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 0, 0, 0))
        moesi.clock.step()
        result = moesi.io.cacheOutput(1).peekInt()
      } while (!moesi.io.procResp(1).peekBoolean())
      println("result: ", result)

      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 0, 0, 0), Seq(0, 3, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(1).peekBoolean())

      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 0, 0, 0), Seq(0, 4, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(1).peekBoolean())

      do {
        pokeProc(moesi, Seq(2, 0, 0, 0), Seq(0, 0, 0, 0), Seq(5, 0, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(0).peekBoolean())

      do {
        pokeProc(moesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(6, 0, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(0).peekBoolean())

      do {
        pokeProc(moesi, Seq(2, 0, 0, 0), Seq(0, 0, 0, 0), Seq(7, 0, 0, 0))
        moesi.clock.step()
      } while (!moesi.io.procResp(0).peekBoolean())
      endSign(moesi)
    }
  }

  "Test" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
//      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(15, 0, 0, 0))
//      moesi.clock.step()
////      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(15, 0, 0, 0))
////      moesi.clock.step()
////      pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))
////      moesi.clock.step()
//
//      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(2, 0, 0, 0))
//      moesi.clock.step()
////      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(2, 0, 0, 0))
////      moesi.clock.step()
////      pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 2, 0, 0))
////      moesi.clock.step()
//
//      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(3, 0, 0, 0))
//      moesi.clock.step()
////      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(3, 0, 0, 0))
////      moesi.clock.step()
////      pokeProc(moesi, Seq(0, 1, 0, 0), Seq(3, 0, 0, 0))
////      moesi.clock.step()
//
//      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(2, 0, 0, 0))
//      moesi.clock.step()
////      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(2, 0, 0, 0))
////      moesi.clock.step()
////      pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 2, 0, 0))
////      moesi.clock.step()
//
//      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
//      moesi.clock.step()
//      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
//      moesi.clock.step()
//      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
//      moesi.clock.step()
//      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
//      moesi.clock.step()
//      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
//      moesi.clock.step()

      var step = 0
      do {
        println(s"${step}----------------------------------")
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 0, 0, 0), Seq(0, 3, 0, 0))
        moesi.clock.step()
        step += 1
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        println(s"${step}----------------------------------")
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 4, 0, 0), Seq(0, 4, 0, 0))
        moesi.clock.step()
        step += 1
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        println(s"${step}----------------------------------")
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 8, 0, 0), Seq(0, 5, 0, 0))
        moesi.clock.step()
        step += 1
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        println(s"${step}----------------------------------")
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 4, 0, 0), Seq(0, 6, 0, 0))
        moesi.clock.step()
        step += 1
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        println(s"${step}----------------------------------")
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 8, 0, 0), Seq(0, 7, 0, 0))
        moesi.clock.step()
        step += 1
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())
      endSign(moesi)
    }
  }

  // processor0 writes 15 into addr 1 (Invalidated -> Modified)
  // processor0 reads from addr 1 (expects 15)
  "W&R" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(15, 0, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (!moesi.io.procResp(0).peekBoolean())

      step = 0
      do {
        pokeProc(moesi, Seq(1, 0, 0, 0), Seq(1, 0, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(0).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(0).peekBoolean())
      println("cache0 output = " + result)

      endSign(moesi)
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  "R" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      for (addr <- 1 until 2) {
        var result: BigInt = 0
        var step: Int = 0
        do {
          pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, addr, 0, 0))

          step += 1
          println("---------------------------------------------------")
          println("step: " + step)
          moesi.clock.step()
          result = moesi.io.cacheOutput(1).peekInt()
          println(s"\tbusData: \n " +
            s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
            s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
            s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
            s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
            s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
            s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
            s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
        } while (step < 20 && !moesi.io.procResp(1).peekBoolean())
        println("cache1 output = " + result)

        endSign(moesi)
      }
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  "R&W" should "pass" in {
    test(new MOESITop).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      endSign(moesi)
      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      endSign(moesi)
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  // processor1 writes 8 into addr 1 (Modified -> Modified)
  "R&W&W" should "pass" in {
    test(new MOESITop).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      endSign(moesi)
      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())
      endSign(moesi)

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 8, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 4 || (step < 20 && !moesi.io.procResp(1).peekBoolean()))

      endSign(moesi)
    }
  }

  // processor1 writes 15 into addr 1 (Invalidated -> Modified)
  // processor2 reads from addr 1 (Invalidated -> Shared, Modified -> Owned, expects 15 from cache1)
  "W&2R" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (!moesi.io.procResp(1).peekBoolean())
      endSign(moesi)

      step = 0
      do {
        pokeProc(moesi, Seq(0, 0, 1, 0), Seq(0, 0, 1, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(2).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(2).peekBoolean())
      println("cache2 output = " + result)

      endSign(moesi)
    }
  }

  // processor1 writes 15 into addr 1 (Invalidated -> Modified)
  // processor2 reads from addr 1 (Invalidated -> Shared, Modified -> Owned, expects 15 from cache1)
  // processor1 writes 8 into addr 1 (Owned -> Exclusive)
  // memory writes 8 into addr 1
  "W&2R&W" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())
      endSign(moesi)

      step = 0
      do {
        pokeProc(moesi, Seq(0, 0, 1, 0), Seq(0, 0, 1, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(2).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(2).peekBoolean())
      println("cache2 output = " + result)

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 8, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      endSign(moesi)
    }
  }

  // processor1 writes 15 into addr 1 (Invalidated -> Modified)
  // processor2 reads from addr 1 (Invalidated -> Shared, Modified -> Owned, expects 15 from cache1)
  // processor2 writes 8 into addr 1 (Shared -> Exclusive)
  // memory writes 8 into addr 1
  "W&2R&2W" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        pokeProc(moesi, Seq(0, 0, 1, 0), Seq(0, 0, 1, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(2).peekInt()
      } while (step < 20 && !moesi.io.procResp(2).peekBoolean())
      println("cache2 output = " + result)

      endSign(moesi)
      step = 0
      do {
        pokeProc(moesi, Seq(0, 0, 2, 0), Seq(0, 0, 1, 0), Seq(0, 0, 8, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
      } while (step < 20 && !moesi.io.procResp(2).peekBoolean())

      endSign(moesi)
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  // processor2 writes 8 into addr 1 (Invalidated -> Modified)
  "R&W&2W" should "pass" in {
    test(new MOESITop).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(1).peekBoolean())

      step = 0
      do {
        pokeProc(moesi, Seq(0, 0, 2, 0), Seq(0, 0, 1, 0), Seq(0, 0, 8, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 20 && !moesi.io.procResp(2).peekBoolean())
    }
  }
}