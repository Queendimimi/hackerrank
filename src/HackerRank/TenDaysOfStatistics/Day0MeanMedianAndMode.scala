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
object Day0MeanMedianAndMode {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val input = nextInt[Vector](n)

    println(mean(input))
    println(median(input))
    println(mode(input))
  }

  def mean(input: Vector[Int]): Double = {
    input.sum / input.length.toDouble
  }

  def median(input: Vector[Int]): Double = {
    val sorted = input.sorted
    if (input.length % 2 == 0) {
      (sorted(input.length / 2) + sorted(input.length / 2 - 1)) / 2.0
    } else {
      sorted(input.length / 2)
    }
  }

  def mode(input: Vector[Int]): Int = {
    val count = input.view.groupBy(a => a).values.map(array => (array.length, array.head)).toVector

    val maxCount = count.filter(pair => pair._1 == count.maxBy(_._1)._1)

    maxCount.minBy(_._2)._2
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
    if (!INPUT.isEmpty) printCustom(System.currentTimeMillis - s + "ms")
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

  private def printCustom(o: AnyRef*): Unit = {
    println(java.util.Arrays.deepToString(o.toArray))
  }
}