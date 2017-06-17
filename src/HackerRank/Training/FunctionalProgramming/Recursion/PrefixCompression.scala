package HackerRank.Training.FunctionalProgramming.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/17/2017
  */
private object PrefixCompression {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val a = nextString().toVector
    val b = nextString().toVector
    val (aWithoutPrefix, bWithoutPrefix, commonPrefix) = a.commonPrefixWithStripedOriginal(b)
    println(commonPrefix.length + " " + commonPrefix.mkString)
    println(aWithoutPrefix.length + " " + aWithoutPrefix.mkString)
    println(bWithoutPrefix.length + " " + bWithoutPrefix.mkString)
  }

  implicit class SeqImprovements[T](a: Seq[T]) {

    def commonPrefix(b: Seq[T]): Seq[T] = {

      @tailrec
      def recursiveCommonPrefix(indexA: Int = 0,
                                indexB: Int = 0,
                                accumulator: mutable.Builder[T, Seq[T]] = Vector.newBuilder[T]): Seq[T] = {
        if (indexA == a.size || indexB == b.size || a(indexA) != b(indexB)) {
          accumulator.result()
        } else {
          accumulator += a(indexA)
          recursiveCommonPrefix(indexA + 1, indexB + 1, accumulator)
        }
      }

      recursiveCommonPrefix()
    }


    def commonPrefixWithStripedOriginal(b: Seq[T]): (Seq[T], Seq[T], Seq[T]) = {

      @tailrec
      def recursiveCommonPrefix(a: Seq[T] = a,
                                b: Seq[T] = b,
                                accumulator: mutable.Builder[T, Seq[T]] = Vector.newBuilder[T]
                               ): (Seq[T], Seq[T], Seq[T]) = {
        if (a.isEmpty || b.isEmpty || a.head != b.head) {
          (a, b, accumulator.result())
        } else {
          accumulator += a.head
          recursiveCommonPrefix(a.tail, b.tail, accumulator)
        }
      }

      recursiveCommonPrefix()
    }
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
  private def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
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