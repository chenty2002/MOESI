package MESI

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class PeekTest extends AnyFlatSpec with ChiselScalatestTester with HasMESIParameters {
  def pokeProc(mesi: MESITop, procOp: Seq[Int], addr: Seq[Int], data: Seq[Int] = Seq.fill(4)(0)) = {
    for (i <- 0 until procNum) {
      if (procOp(i) != 0) {
        mesi.io.procOp(i).poke(procOp(i).U(procOpBits.W))
        mesi.io.addr(i).poke(addr(i).U(addrBits.W))
        mesi.io.cacheInput(i).poke(data(i).U(cacheBlockBits.W))
      } else {
        mesi.io.procOp(i).poke(0.U(procOpBits.W))
        mesi.io.addr(i).poke(0.U(addrBits.W))
        mesi.io.cacheInput(i).poke(0.U(cacheBlockBits.W))
      }
    }
  }

  // processor0 writes 15 into addr 1 (Invalidated -> Modified)
  // processor0 reads from addr 1 (expects 15)
  "W&R" should "pass" in {
    test(new MESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(mesi, Seq(2, 0, 0, 0), Seq(1, 0, 0, 0), Seq(15, 0, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(0).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (mesi.io.procHlt(0).peekBoolean())

      step = 0
      do {
        pokeProc(mesi, Seq(1, 0, 0, 0), Seq(1, 0, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(0).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && (result == 0 || mesi.io.procHlt(0).peekBoolean()))
      println("cache0 output = " + result)
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  "R" should "pass" in {
    test(new MESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      for (addr <- 1 until 2) {
        var result: BigInt = 0
        var step: Int = 0
        var flag: Boolean = true
        var prHlt: Boolean = false
        do {
          pokeProc(mesi, Seq(0, 1, 0, 0), Seq(0, addr, 0, 0))

          step += 1
          println("-------------------------------------")
          println("step: " + step)
          mesi.clock.step()
          //          println(s"addr: ${addr}")
          result = mesi.io.cacheOutput(1).peekInt()
          println(s"busHold: ${mesi.io.busHold.peekBoolean()}")
          println(s"busValid: ${mesi.io.busValid.peekBoolean()}")
          println(s"proc0Valid: ${mesi.io.procValid.peekBoolean()}")
          println(s"\tbusData: \n " +
            s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
            s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
            s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
            s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
            s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
            s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
            s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}\n " +
            s"\t\taddr: ${mesi.io.busAddr.peekInt()}\n " +
            s"\t\taddrEq: ${mesi.io.addrEq.peekBoolean()}")
          println(s"\tmemData: \n " +
            s"\t\tpid: ${mesi.io.memory.pid.peekInt()}\n " +
            s"\t\ttrans: ${mesi.io.memory.busTransaction.peekInt()}\n " +
            s"\t\ttag: ${mesi.io.memory.tag.peekInt()}\n " +
            s"\t\tindex: ${mesi.io.memory.index.peekInt()}\n " +
            s"\t\tcacheBlock: ${mesi.io.memory.cacheBlock.peekInt()}\n " +
            s"\t\tstate: ${mesi.io.memory.state.peekInt()}\n " +
            s"\t\tvalid: ${mesi.io.memory.valid.peekBoolean()}\n " +
            s"\t\taddr: ${mesi.io.memAddr.peekInt()}")
          println(s"\tcacheStatus(1)(1): ${mesi.io.cacheStatus(1)(1).peekInt()}")
          prHlt = mesi.io.procHlt(1).peekBoolean()
          println(s"prHlt: $prHlt\n")
          if (prHlt) flag = false
        } while (step < 10 && (flag || prHlt))
        println("cache0 output = " + result)
      }
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  "R&W" should "pass" in {
    test(new MESITop).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      var result: BigInt = 0
      var step: Int = 0
      var flag: Boolean = true
      var prHlt: Boolean = false
      do {
        pokeProc(mesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("-------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        //          println(s"addr: ${addr}")
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"busHold: ${mesi.io.busHold.peekBoolean()}")
        println(s"busValid: ${mesi.io.busValid.peekBoolean()}")
        println(s"proc0Valid: ${mesi.io.procValid.peekBoolean()}")
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}\n " +
          s"\t\taddr: ${mesi.io.busAddr.peekInt()}\n " +
          s"\t\taddrEq: ${mesi.io.addrEq.peekBoolean()}")
        println(s"\tmemData: \n " +
          s"\t\tpid: ${mesi.io.memory.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.memory.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.memory.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.memory.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.memory.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.memory.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.memory.valid.peekBoolean()}\n " +
          s"\t\taddr: ${mesi.io.memAddr.peekInt()}")
        println(s"\tcacheStatus(1)(1): ${mesi.io.cacheStatus(1)(1).peekInt()}")
        prHlt = mesi.io.procHlt(1).peekBoolean()
        println(s"prHlt: $prHlt\n")
        if (prHlt) flag = false
      } while (step < 10 && (flag || prHlt))

      step = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && mesi.io.procHlt(1).peekBoolean())
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  // processor1 writes 8 into addr 1 (Modified -> Modified)
  "R&W&W" should "pass" in {
    test(new MESITop).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      var result: BigInt = 0
      var step: Int = 0
      var flag: Boolean = true
      var prHlt: Boolean = false
      do {
        pokeProc(mesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("-------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        //          println(s"addr: ${addr}")
        println(s"busHold: ${mesi.io.busHold.peekBoolean()}")
        println(s"busValid: ${mesi.io.busValid.peekBoolean()}")
        println(s"proc0Valid: ${mesi.io.procValid.peekBoolean()}")
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}\n " +
          s"\t\taddr: ${mesi.io.busAddr.peekInt()}\n " +
          s"\t\taddrEq: ${mesi.io.addrEq.peekBoolean()}")
        println(s"\tmemData: \n " +
          s"\t\tpid: ${mesi.io.memory.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.memory.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.memory.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.memory.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.memory.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.memory.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.memory.valid.peekBoolean()}\n " +
          s"\t\taddr: ${mesi.io.memAddr.peekInt()}")
        println(s"\tcacheStatus(1)(1): ${mesi.io.cacheStatus(1)(1).peekInt()}")
        prHlt = mesi.io.procHlt(1).peekBoolean()
        println(s"prHlt: $prHlt\n")
        if (prHlt) flag = false
      } while (step < 10 && (flag || prHlt))

      step = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && mesi.io.procHlt(1).peekBoolean())

      step = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 8, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && mesi.io.procHlt(1).peekBoolean())
    }
  }

  // processor1 writes 15 into addr 1 (Invalidated -> Modified)
  // processor2 reads from addr 1 (Invalidated -> Shared, expects 15 from cache1)
  // memory writes 15 into addr 1 (from processor 1)
  "W&2R" should "pass" in {
    test(new MESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (mesi.io.procHlt(1).peekBoolean())

      //      println("---------------------------------------------------")
      //      pokeProc(mesi, Seq(0,0,0,0), Seq(0,0,0,0))
      //      mesi.clock.step()
      step = 0
      do {
        pokeProc(mesi, Seq(0, 0, 1, 0), Seq(0, 0, 1, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(2).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && (result == 0 || mesi.io.procHlt(2).peekBoolean()))
      println("cache0 output = " + result)
    }
  }

  // processor1 writes 15 into addr 1 (Invalidated -> Modified)
  // processor2 reads from addr 1 (Invalidated -> Shared, expects 15 from cache1)
  // memory writes 15 into addr 1 (from processor 1)
  // processor1 writes 8 into addr 1 (Shared -> Exclusive)
  // memory writes 8 into addr 1 (from processor 1)
  "W&2R&W" should "pass" in {
    test(new MESITop()).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      var result: BigInt = 0
      var step: Int = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (mesi.io.procHlt(1).peekBoolean())

      step = 0
      do {
        pokeProc(mesi, Seq(0, 0, 1, 0), Seq(0, 0, 1, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(2).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && (result == 0 || mesi.io.procHlt(2).peekBoolean()))
      println("cache0 output = " + result)

      step = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 8, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (mesi.io.procHlt(1).peekBoolean())
    }
  }

  // processor1 reads from addr 1 (Invalidated -> Exclusive, expects 0 from memory)
  // processor1 writes 15 into addr 1 (Exclusive -> Modified)
  // processor2 writes 8 into addr 1 (Invalidated -> Exclusive)
  // memory writes 8 into addr 1 (from processor2)
  "R&W&2W" should "pass" in {
    test(new MESITop).withAnnotations(Seq(WriteVcdAnnotation)) { mesi =>
      var result: BigInt = 0
      var step: Int = 0
      var flag: Boolean = true
      var prHlt: Boolean = false
      do {
        pokeProc(mesi, Seq(0, 1, 0, 0), Seq(0, 1, 0, 0))

        step += 1
        println("-------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        //          println(s"addr: ${addr}")
        println(s"busHold: ${mesi.io.busHold.peekBoolean()}")
        println(s"busValid: ${mesi.io.busValid.peekBoolean()}")
        println(s"proc0Valid: ${mesi.io.procValid.peekBoolean()}")
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}\n " +
          s"\t\taddr: ${mesi.io.busAddr.peekInt()}\n " +
          s"\t\taddrEq: ${mesi.io.addrEq.peekBoolean()}")
        println(s"\tmemData: \n " +
          s"\t\tpid: ${mesi.io.memory.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.memory.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.memory.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.memory.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.memory.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.memory.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.memory.valid.peekBoolean()}\n " +
          s"\t\taddr: ${mesi.io.memAddr.peekInt()}")
        println(s"\tcacheStatus(1)(1): ${mesi.io.cacheStatus(1)(1).peekInt()}")
        prHlt = mesi.io.procHlt(1).peekBoolean()
        println(s"prHlt: $prHlt\n")
        if (prHlt) flag = false
      } while (step < 10 && (flag || prHlt))

      step = 0
      do {
        pokeProc(mesi, Seq(0, 2, 0, 0), Seq(0, 1, 0, 0), Seq(0, 15, 0, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(1).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && mesi.io.procHlt(1).peekBoolean())

      step = 0
      do {
        pokeProc(mesi, Seq(0, 0, 2, 0), Seq(0, 0, 1, 0), Seq(0, 0, 8, 0))

        step += 1
        println("---------------------------------------------------")
        println("step: " + step)
        mesi.clock.step()
        result = mesi.io.cacheOutput(2).peekInt()
        println(s"\tbusData: \n " +
          s"\t\tpid: ${mesi.io.busData.pid.peekInt()}\n " +
          s"\t\ttrans: ${mesi.io.busData.busTransaction.peekInt()}\n " +
          s"\t\ttag: ${mesi.io.busData.tag.peekInt()}\n " +
          s"\t\tindex: ${mesi.io.busData.index.peekInt()}\n " +
          s"\t\tcacheBlock: ${mesi.io.busData.cacheBlock.peekInt()}\n " +
          s"\t\tstate: ${mesi.io.busData.state.peekInt()}\n " +
          s"\t\tvalid: ${mesi.io.busData.valid.peekBoolean()}")
      } while (step < 10 && mesi.io.procHlt(2).peekBoolean())
    }
  }
}

/*
0 -> 1: inside l1, validateBus = true
1 -> 2: busDate updated by l1(0), memData read with busData
 */