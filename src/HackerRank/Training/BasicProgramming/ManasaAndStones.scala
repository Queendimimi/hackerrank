package HackerRank.Training.BasicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/12/2017
  */
object ManasaAndStones {
  private val INPUT = "2\n6\n4\n8\n11\n3\n10"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()
    val input = next[(Int, Int, Int), Vector]((nextInt(), nextInt(), nextInt()), t)
    input.foreach { case (n, a, b) =>
      println(lastStones(a, b, n).mkString(" "))
    }
  }

  private def lastStones(a: Int, b: Int, n: Int) = {
    val builder = Set.newBuilder[Int]

    @tailrec
    def nextStones(previousStones: Set[Int], depth: Int): Unit = {
      val next = previousStones.foldLeft(Set.newBuilder[Int]) { case (acm, value) =>
        acm += value + a
        acm += value + b
      }

      if (n - 1 == depth) {
        builder ++= next.result()
      } else {
        nextStones(next.result(), depth + 1)
      }
    }

    nextStones(Set(0), 1)
    builder.result().toVector.sorted
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

  private def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
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