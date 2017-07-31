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
private[this] object StringMingling {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val a = nextString().to[Vector]
    val b = nextString()

    println((a interleave b).mkString(""))
  }

  implicit class SeqImprovements[T](a: Seq[T]) {

    def interleave(b: Seq[T])(implicit cbf: CanBuildFrom[Seq[T], T, Seq[T]]): Seq[T] = {
      require(a.size == b.size)

      @tailrec
      def recursiveInterleave(indexA: Int = 0,
                              indexB: Int = 0,
                              accumulator: mutable.Builder[T, Seq[T]] = cbf()): Seq[T] = {
        if (indexA == a.size) {
          accumulator.result()
        } else {
          accumulator += a(indexA) += b(indexB)
          recursiveInterleave(indexA + 1, indexB + 1, accumulator)
        }
      }

      recursiveInterleave()
    }
  }

  implicit class IterableImprovements[T](a: Iterable[T]) {

    def interleave(b: Iterable[T])(implicit cbf: CanBuildFrom[Iterable[T], T, Iterable[T]]): Iterable[T] = {
      require(a.size == b.size)

      @tailrec
      def recursiveInterleave(a: Iterable[T],
                              b: Iterable[T],
                              accumulator: mutable.Builder[T, Iterable[T]] = cbf()): Iterable[T] = {
        if (a.isEmpty) {
          accumulator.result()
        } else {
          accumulator += a.head += b.head
          recursiveInterleave(a.tail, b.tail, accumulator)
        }
      }

      recursiveInterleave(a, b)
    }
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
  private[this] def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
  }

  private[this] def nextString(): String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) {
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
  }

  private[this] val inputBuffer = new Array[Byte](1024)
  private[this] var lenBuffer = 0
  private[this] var ptrBuffer = 0

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

  private[this] def isSpaceChar(c: Int) = !(c >= 33 && c <= 126)

  private[this] def skip = {
    var b = 0
    while ( {
      b = readByte()
      b != -1 && isSpaceChar(b)
    }) {}
    b
  }
}