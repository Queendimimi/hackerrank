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
object Euler009SpecialPythagoreanTriplet {
  private val INPUT = "1\n1000"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    nextInt[Array](n).foreach(x => out.println(findMaxTriplet(x)))
  }

  private def findMaxTriplet(n: Int) = {
    var max = Int.MinValue
    for (a <- 3 to (n - 3) / 3) {
      for (b <- a + 1 to (n - 1 - a) / 2) {
        val c = n - a - b
        if (isTriplet(a, b, c) && a * b * c > max) {
          max = a * b * c
        }
      }
    }
    if (max == Int.MinValue) -1 else max
  }

  private def isTriplet(a: Int, b: Int, c: Int) = {
    a * a + b * b == c * c
  }

  //------------------------------------------------------------------------------------------//
  // Input-Output                                                                 
  //------------------------------------------------------------------------------------------//
  var in: java.io.InputStream = _
  var out: java.io.PrintWriter = _

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

  private def nextSeq[T, Coll[_]](reader: => Seq[T], n: Int)
                                 (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder ++= reader
    }
    builder.result()
  }

  private def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
    }
    builder.result()
  }

  private def nextWithIndex[T, Coll[_]](reader: => T, n: Int)
                                       (implicit cbf: CanBuildFrom[Coll[(T, Int)], (T, Int), Coll[(T, Int)]]): Coll[(T, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((reader, i))
    }
    builder.result()
  }

  private def nextDouble[Coll[Double]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Double], Double, Coll[Double]]): Coll[Double] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextDouble()
    }
    builder.result()
  }

  private def nextDoubleWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Double, Int)], (Double, Int), Coll[(Double, Int)]]): Coll[(Double, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextDouble(), i))
    }
    builder.result()
  }

  private def nextChar[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Char], Char, Coll[Char]]): Coll[Char] = {
    val builder = cbf()
    builder.sizeHint(n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      builder += b.toChar
      p += 1
      b = readByte()
    }
    builder.result()
  }

  private def nextCharWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Char, Int)], (Char, Int), Coll[(Char, Int)]]): Coll[(Char, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      builder += ((b.toChar, p))
      p += 1
      b = readByte()
    }
    builder.result()
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

  private def nextIntWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Int, Int)], (Int, Int), Coll[(Int, Int)]]): Coll[(Int, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextInt(), i))
    }
    builder.result()
  }

  private def nextLong[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Long], Long, Coll[Long]]): Coll[Long] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextLong()
    }
    builder.result()
  }

  private def nextLongWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Long, Int)], (Long, Int), Coll[(Long, Int)]]): Coll[(Long, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextLong(), i))
    }
    builder.result()
  }

  private def nextString[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[String], String, Coll[String]]): Coll[String] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextString()
    }
    builder.result()
  }

  private def nextStringWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(String, Int)], (String, Int), Coll[(String, Int)]]): Coll[(String, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextString(), i))
    }
    builder.result()
  }

  private def nextMultiLine(n: Int, m: Int): Array[Array[Char]] = {
    val map = new Array[Array[Char]](n)
    var i = 0
    while (i < n) {
      map(i) = nextChar[Array](m)
      i += 1
    }
    map
  }

  private def nextDouble(): Double = nextString().toDouble

  private def nextChar(): Char = skip.toChar

  private def nextString(): String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) {
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
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

  private def nextLong(): Long = {
    var num = 0L
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
    throw new IOException("Read Long")
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
    System.out.println(java.util.Arrays.deepToString(o.toArray)
    )
  }
}