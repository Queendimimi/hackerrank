package HackerRank.NSECodesprint

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/26/2017
  */
object CrossOver {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val prices = nextInt[Array](n)

    val result = crossOverTriplet(prices)

    if (result.nonEmpty) {
      println(result.map(_.productIterator.mkString(" ")).mkString("\n"))
    }
  }

  def shortTermMovingAvg(prices: Array[Int]): Iterator[Double] = {
    prices.sliding(60).map(_.sum / 60.0)
  }

  def longTermMovingAvg(prices: Array[Int]): Iterator[Double] = {
    prices.sliding(300).map(_.sum / 300.0)
  }

  def combine(shortTermMovingAvg: Iterator[Double], longTermMovingAvg: Iterator[Double]): Iterator[(Int, (Double, Double))] = {
    shortTermMovingAvg.drop(240).zip(longTermMovingAvg).zipWithIndex.map(pair =>
      (pair._2 + 300, pair._1))
  }

  def isCrossOver(previous: (Double, Double), current: (Double, Double)): Boolean = {
    val (previousShortTerm, previousLongTerm) = previous
    val (currentShortTerm, currentLongTerm) = current
    if (previousShortTerm > previousLongTerm && currentShortTerm <= currentLongTerm) {
      true
    } else if (previousShortTerm < previousLongTerm && currentShortTerm >= currentLongTerm) {
      true
    } else if (previousShortTerm == previousLongTerm && currentShortTerm != currentLongTerm) {
      true
    } else {
      false
    }
  }

  def round(input: Double): Double = {
    Math.round(input * 100.0) / 100.0
  }

  def crossOver(shortTermMovingAvg: Iterator[Double], longTermMovingAvg: Iterator[Double]): Iterator[(Int, (Double, Double))] = {
    val combined = combine(shortTermMovingAvg, longTermMovingAvg)

    combined.sliding(2).map(pair => (pair.head, pair.last)).filter {
      case (previous, current) =>
        isCrossOver(previous._2, current._2)
    }.map(_._2)
  }

  def crossOverTriplet(prices: Array[Int]): Iterator[(Int, String, String)] = {
    crossOver(shortTermMovingAvg(prices), longTermMovingAvg(prices)).map(pair =>
      (pair._1, f"${round(pair._2._1)}%1.2f", f"${round(pair._2._2)}%1.2f"))
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