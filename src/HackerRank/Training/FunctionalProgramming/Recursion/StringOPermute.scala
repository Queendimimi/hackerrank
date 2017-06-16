package HackerRank.Training.FunctionalProgramming.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/16/2017
  */
private object StringOPermute {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()

    next[Unit, Vector](out.println(nextString().to[Vector].swapEven.mkString("")), t)
  }

  implicit class SeqImprovements[T](a: Seq[T]) {

    def swapEven()(implicit cbf: CanBuildFrom[Seq[T], T, Seq[T]]): Seq[T] = {

      @tailrec
      def recursiveSwap(index: Int = 0,
                        accumulator: mutable.Builder[T, Seq[T]] = cbf()): Seq[T] = {
        if (index == a.size - 1) {
          (accumulator += a(index)).result()
        } else if (index >= a.size) {
          accumulator.result()
        } else {
          accumulator += a(index + 1) += a(index)
          recursiveSwap(index + 2, accumulator)
        }
      }

      recursiveSwap()
    }
  }

  //------------------------------------------------------------------------------------------//
  // Input-Output                                                                 
  //------------------------------------------------------------------------------------------//
  private var in: java.io.InputStream = _
  private var out: java.io.PrintWriter = _

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    run()
  }

  @throws[Exception]
  private def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
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

  private val inputBuffer = new Array[Byte](1024)
  private var lenBuffer = 0
  private var ptrBuffer = 0

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
}