package HackerRank.TenDaysOfStatistics

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/26/2017
  */
object Day1InterquartileRange {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val x = nextInt[Vector](n)
    val frequency = nextInt[Vector](n)
    val builder = Vector.newBuilder[Int]
    for (i <- 0 until n) {
      for (_ <- 0 until frequency(i)) {
        builder += x(i)
      }
    }
    val result = quartiles(builder.result().sorted)
    println(round(result._3 - result._1))
  }

  private def quartiles(input: Vector[Int]) = {
    val q50 = median(input)
    val (left, right) = splitAtMedian(input)
    val q25 = median(left)
    val q75 = median(right)
    (q25, q50, q75)
  }

  private def splitAtMedian(input: Vector[Int]) = {
    if (input.length % 2 == 0) {
      input.splitAt(input.length / 2)
    } else {
      val split = input.splitAt(input.length / 2)
      (split._1, split._2.drop(1))
    }
  }

  def median(input: Vector[Int]): Double = {
    if (input.length % 2 == 0) {
      (input(input.length / 2) + input(input.length / 2 - 1)) / 2.0
    } else {
      input(input.length / 2)
    }
  }

  def round(input: Double): Double = {
    Math.round(input * 10.0) / 10.0
  }


  //------------------------------------------------------------------------------------------//
  // Input-Output                                                                 
  //------------------------------------------------------------------------------------------//
  private var in: java.io.InputStream = _
  private var out: java.io.PrintWriter = _

  private def println(x: Any) = out.println(x)

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    run()
  }

  @throws[Exception]
  def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
  }

  private def nextInt[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
    }
    builder.result()
  }

  private def nextInt(): Int = {
    var num = 0
    var b = 0
    var minus = false
    while ( {
      b = readByte()
      b != -1 && !((b >= '0' && b <= '9') || b == '-')
    }) {}
    if (b == '-') {
      minus = true
      b = readByte()
    }
    while (true) {
      if (b >= '0' && b <= '9') {
        num = num * 10 + (b - '0')
      } else {
        if (minus) return -num else return num
      }
      b = readByte()
    }
    throw new IOException("Read Int")
  }

  private val inputBuffer = new Array[Byte](1024)
  var lenBuffer = 0
  var ptrBuffer = 0

  private def readByte(): Int = {
    if (lenBuffer == -1) throw new InputMismatchException
    if (ptrBuffer >= lenBuffer) {
      ptrBuffer = 0
      try {
        lenBuffer = in.read(inputBuffer)
      } catch {
        case _: IOException =>
          throw new InputMismatchException
      }
      if (lenBuffer <= 0) return -1
    }
    inputBuffer({
      ptrBuffer += 1
      ptrBuffer - 1
    })
  }
}