package HackerRank.ProjectEuler

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/23/2017
  */
object Euler007BigPrimes {
  private[this] val INPUT = "1\n10001"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val n = nextInt()
    nextInt[Array](n).foreach(x => println(nthPrime(x)))
  }

  // Count number of set bits in an int
  private[this] def popCount(number: Int): Int = {
    var n = number
    n -= (n >>> 1) & 0x55555555
    n = ((n >>> 2) & 0x33333333) + (n & 0x33333333)
    n = ((n >> 4) & 0x0F0F0F0F) + (n & 0x0F0F0F0F)
    (n * 0x01010101) >> 24
  }

  private[this] def nthPrime(n: Int): Int = {
    if (n < 2) return 2
    if (n == 2) return 3
    if (n == 3) return 5
    var limit = 0
    var root = 0
    var count = 2
    limit = (n * (Math.log(n) + Math.log(Math.log(n)))).toInt + 3
    root = Math.sqrt(limit).toInt
    limit % 6 match {
      case 0 => limit = 2 * (limit / 6) - 1
      case 5 => limit = 2 * (limit / 6) + 1
      case _ => limit = 2 * (limit / 6)
    }
    root % 6 match {
      case 0 => root = 2 * (root / 6) - 1
      case 5 => root = 2 * (root / 6) + 1
      case _ => root = 2 * (root / 6)
    }
    val dim = (limit + 31) >> 5
    val sieve = new Array[Int](dim)
    var i = 0
    while (i < root) {
      if ((sieve(i >> 5) & (1 << (i & 31))) == 0) {
        var start = 0
        var s1 = 0
        var s2 = 0
        if ((i & 1) == 1) {
          start = i * (3 * i + 8) + 4
          s1 = 4 * i + 5
          s2 = 2 * i + 3
        } else {
          start = i * (3 * i + 10) + 7
          s1 = 2 * i + 3
          s2 = 4 * i + 7
        }
        var j = start
        while (j < limit) {
          sieve(j >> 5) |= 1 << (j & 31)
          j += s1
          if (j >= limit) {} else {
            sieve(j >> 5) |= 1 << (j & 31)
            j += s2
          }
        }
      }
      i += 1
    }
    i = 0
    while (count < n) {
      count += popCount(~sieve(i))
      i += 1
    }
    i -= 1
    val mask = ~sieve(i)
    var p = 0
    p = 31
    while (count >= n) {
      count -= (mask >> p) & 1
      p -= 1
    }
    3 * (p + (i << 5)) + 7 + (p & 1)
  }

  //------------------------------------------------------------------------------------------//
  // Input-Output
  //------------------------------------------------------------------------------------------//
  private[this] var in: java.io.InputStream = _
  private[this] var out: java.io.PrintWriter = _

  private[this] def println(x: Any) = out.println(x)

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

  private[this] def nextInt[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
    }
    builder.result()
  }

  private[this] def nextInt(): Int = {
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

  private[this] val inputBuffer = new Array[Byte](1024)
  var lenBuffer = 0
  var ptrBuffer = 0

  private[this] def readByte(): Int = {
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