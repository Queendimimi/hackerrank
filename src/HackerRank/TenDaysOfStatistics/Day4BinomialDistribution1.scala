package HackerRank.TenDaysOfStatistics

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 5/9/2017
  */
object Day4BinomialDistribution1 {
  private val INPUT = "1.09 1"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val boys = nextDouble()
    val girls = nextDouble()
    val odds = boys / (boys + girls)
    println(Math.rint((3 to 6).map(binomialDist(6, _, odds)).sum * 1000) / 1000)
  }


  private def power(n: Double, i: Int) = {
    @tailrec
    def _power(n: Double, i: Int, current: Double): Double = {
      if (i == 1) {
        current
      } else {
        _power(n, i - 1, current * n)
      }
    }

    if (i == 0) 1.0 else _power(n, i, n)
  }

  private def factorial(n: Int): Int = {
    if (n == 0) 1 else n * factorial(n - 1)
  }

  private def combination(n: Int, k: Int) = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }

  private def binomialDist(n: Int, x: Int, p: Double) = {
    combination(n, x) * power(p, x) * power(1 - p, n - x)
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

  private def nextDouble(): Double = nextString().toDouble

  private def nextString(): String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) {
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
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

  private def isSpaceChar(c: Int) = !(c >= 33 && c <= 126)

  private def skip = {
    var b = 0
    while ( {
      b = readByte()
      b != -1 && isSpaceChar(b)
    }) {}
    b
  }

  private def printCustom(o: AnyRef*): Unit = {
    println(java.util.Arrays.deepToString(o.toArray))
  }
}