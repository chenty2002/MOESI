package MOESI

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class PeekTest extends AnyFlatSpec with ChiselScalatestTester with HasMOESIParameters {
  def pokeProc(moesi: MOESITop, procOp: Seq[Int], addr: Seq[Int], data: Seq[Int] = Seq.fill(4)(0)) = {
    for (i <- 0 until procNum) {
      if (procOp(i) != 0) {
        moesi.io.procOp(i).poke(procOp(i).U(procOpBits.W))
        moesi.io.addr(i).poke(addr(i).U(addrBits.W))
        moesi.io.cacheInput(i).poke(data(i).U(cacheBlockBits.W))
      } else {
        moesi.io.procOp(i).poke(0.U(procOpBits.W))
        moesi.io.addr(i).poke(0.U(addrBits.W))
        moesi.io.cacheInput(i).poke(0.U(cacheBlockBits.W))
      }
    }
  }

  "Test" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(15, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(15, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(2, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(2, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(3, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(1, 0, 0, 0), Seq(3, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(2, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(2, 0, 0, 0), Seq(2, 0, 0, 0))
      moesi.clock.step()

      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
      moesi.clock.step()
      pokeProc(moesi, Seq(0, 0, 0, 0), Seq(0, 0, 0, 0))
      moesi.clock.step()
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
        result = moesi.io.cacheOutput(0).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 2 || moesi.io.procHlt(0).peekBoolean())

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
      } while (step < 2 || (step < 20 && (result == 0 || moesi.io.procHlt(0).peekBoolean())))
      println("cache0 output = " + result)
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  "R" should "pass" in {
    test(new MOESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      for (addr <- 1 until 2) {
        var result: BigInt = 0
        var step: Int = 0
        var flag: Boolean = true
        var prHlt: Boolean = false
        do {
          pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, addr, 0, 0))

          step += 1
          println("-------------------------------------")
          println("step: " + step)
          moesi.clock.step()
          //          println(s"addr: ${addr}")
          result = moesi.io.cacheOutput(1).peekInt()
          println(s"busHold: ${moesi.io.busHold.peekBoolean()}")
          println(s"busValid: ${moesi.io.busValid.peekBoolean()}")
          println(s"proc0Valid: ${moesi.io.procValid.peekBoolean()}")
          println(s"\tbusData: \n " +
            s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
            s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
            s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
            s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
            s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
            s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
            s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}\n " +
            s"\t\taddr: ${moesi.io.busAddr.peekInt()}\n " +
            s"\t\taddrEq: ${moesi.io.addrEq.peekBoolean()}")
          println(s"\tmemData: \n " +
            s"\t\tpid: ${moesi.io.memory.pid.peekInt()}\n " +
            s"\t\ttrans: ${moesi.io.memory.busTransaction.peekInt()}\n " +
            s"\t\ttag: ${moesi.io.memory.tag.peekInt()}\n " +
            s"\t\tindex: ${moesi.io.memory.index.peekInt()}\n " +
            s"\t\tcacheBlock: ${moesi.io.memory.cacheBlock.peekInt()}\n " +
            s"\t\tstate: ${moesi.io.memory.state.peekInt()}\n " +
            s"\t\tvalid: ${moesi.io.memory.valid.peekBoolean()}\n " +
            s"\t\taddr: ${moesi.io.memAddr.peekInt()}")
          println(s"\tcacheStatus(1)(1): ${moesi.io.cacheStatus(1)(1).peekInt()}")
          prHlt = moesi.io.procHlt(1).peekBoolean()
          println(s"prHlt: $prHlt\n")
          if (prHlt) flag = false
        } while (step < 20 && (flag || prHlt))
        println("cache0 output = " + result)
      }
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  "R&W" should "pass" in {
    test(new MOESITop).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      var flag: Boolean = true
      var prHlt: Boolean = false
      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("-------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        //          println(s"addr: ${addr}")
        result = moesi.io.cacheOutput(1).peekInt()
        println(s"busHold: ${moesi.io.busHold.peekBoolean()}")
        println(s"busValid: ${moesi.io.busValid.peekBoolean()}")
        println(s"proc0Valid: ${moesi.io.procValid.peekBoolean()}")
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}\n " +
          s"\t\taddr: ${moesi.io.busAddr.peekInt()}\n " +
          s"\t\taddrEq: ${moesi.io.addrEq.peekBoolean()}")
        println(s"\tmemData: \n " +
          s"\t\tpid: ${moesi.io.memory.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.memory.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.memory.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.memory.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.memory.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.memory.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.memory.valid.peekBoolean()}\n " +
          s"\t\taddr: ${moesi.io.memAddr.peekInt()}")
        println(s"\tcacheStatus(1)(1): ${moesi.io.cacheStatus(1)(1).peekInt()}")
        prHlt = moesi.io.procHlt(1).peekBoolean()
        println(s"prHlt: $prHlt\n")
        if (prHlt) flag = false
      } while (step < 20 && (flag || prHlt))

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

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
      } while (step < 2 || (step < 20 && moesi.io.procHlt(1).peekBoolean()))
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  // processor1 writes 8 into addr 1 (Modified -> Modified)
  "R&W&W" should "pass" in {
    test(new MOESITop).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      var flag: Boolean = true
      var prHlt: Boolean = false
      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("-------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(1).peekInt()
        //          println(s"addr: ${addr}")
        println(s"busHold: ${moesi.io.busHold.peekBoolean()}")
        println(s"busValid: ${moesi.io.busValid.peekBoolean()}")
        println(s"proc0Valid: ${moesi.io.procValid.peekBoolean()}")
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}\n " +
          s"\t\taddr: ${moesi.io.busAddr.peekInt()}\n " +
          s"\t\taddrEq: ${moesi.io.addrEq.peekBoolean()}")
        println(s"\tmemData: \n " +
          s"\t\tpid: ${moesi.io.memory.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.memory.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.memory.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.memory.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.memory.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.memory.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.memory.valid.peekBoolean()}\n " +
          s"\t\taddr: ${moesi.io.memAddr.peekInt()}")
        println(s"\tcacheStatus(1)(1): ${moesi.io.cacheStatus(1)(1).peekInt()}")
        prHlt = moesi.io.procHlt(1).peekBoolean()
        println(s"prHlt: $prHlt\n")
        if (prHlt) flag = false
      } while (step < 2 || (step < 20 && (flag || prHlt)))

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

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
      } while (step < 2 || (step < 20 && moesi.io.procHlt(1).peekBoolean()))

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 8, 0, 0))

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
      } while (step < 2 || (step < 20 && moesi.io.procHlt(1).peekBoolean()))
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
        result = moesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 2 || moesi.io.procHlt(1).peekBoolean())

      //      println("---------------------------------------------------")
      //      pokeProc(moesi, Seq(0,0,0,0), Seq(0,0,0,0))
      //      moesi.clock.step()
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
      } while (step < 2 || (step < 20 && (result == 0 || moesi.io.procHlt(2).peekBoolean())))
      println("cache0 output = " + result)
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
        result = moesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}")
      } while (step < 2 || moesi.io.procHlt(1).peekBoolean())

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
      } while (step < 2 || (step < 20 && (result == 0 || moesi.io.procHlt(2).peekBoolean())))
      println("cache0 output = " + result)

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 8, 0, 0))

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
      } while (step < 2 || moesi.io.procHlt(1).peekBoolean())
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  // processor2 writes 8 into addr 1 (Invalidated -> Modified)
  "R&W&2W" should "pass" in {
    test(new MOESITop).withAnnotations(Seq(WriteVcdAnnotation)) { moesi =>
      var result: BigInt = 0
      var step: Int = 0
      var flag: Boolean = true
      var prHlt: Boolean = false
      do {
        pokeProc(moesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("-------------------------------------")
        println("step: " + step)
        moesi.clock.step()
        result = moesi.io.cacheOutput(1).peekInt()
        //          println(s"addr: ${addr}")
        println(s"busHold: ${moesi.io.busHold.peekBoolean()}")
        println(s"busValid: ${moesi.io.busValid.peekBoolean()}")
        println(s"proc0Valid: ${moesi.io.procValid.peekBoolean()}")
        println(s"\tbusData: \n " +
          s"\t\tpid: ${moesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.busData.valid.peekBoolean()}\n " +
          s"\t\taddr: ${moesi.io.busAddr.peekInt()}\n " +
          s"\t\taddrEq: ${moesi.io.addrEq.peekBoolean()}")
        println(s"\tmemData: \n " +
          s"\t\tpid: ${moesi.io.memory.pid.peekInt()}\n " +
          s"\t\ttrans: ${moesi.io.memory.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${moesi.io.memory.tag.peekInt()}\n " +
          s"\t\tindex: ${moesi.io.memory.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${moesi.io.memory.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${moesi.io.memory.state.peekInt()}\n " +
          s"\t\tvalid: ${moesi.io.memory.valid.peekBoolean()}\n " +
          s"\t\taddr: ${moesi.io.memAddr.peekInt()}")
        println(s"\tcacheStatus(1)(1): ${moesi.io.cacheStatus(1)(1).peekInt()}")
        prHlt = moesi.io.procHlt(1).peekBoolean()
        println(s"prHlt: $prHlt\n")
        if (prHlt) flag = false
      } while (step < 2 || (step < 20 && (flag || prHlt)))

      step = 0
      do {
        pokeProc(moesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

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
      } while (step < 2 || (step < 20 && moesi.io.procHlt(1).peekBoolean()))

      step = 0
      do {
        pokeProc(moesi, Seq(0, 0, 2, 0), Seq(0, 0, 1, 0), Seq(0, 0, 8, 0))

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
      } while (step < 2 || (step < 20 && moesi.io.procHlt(2).peekBoolean()))
    }
  }
}