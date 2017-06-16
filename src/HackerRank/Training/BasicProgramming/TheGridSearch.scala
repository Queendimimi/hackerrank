package HackerRank.Training.BasicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/10/2017
  */
object TheGridSearch {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()
    val input = next[(Vector[Vector[Char]], Vector[Vector[Char]]), Vector]({
      val row = nextInt()
      val column = nextInt()
      val a = next[Vector[Char], Vector](nextChar[Vector](column), row)
      val r = nextInt()
      val c = nextInt()
      val b = next[Vector[Char], Vector](nextChar[Vector](c), r)
      (a, b)
    }, t)

    input.map { case (grid, query) =>
      val searchMatched = grid
        .map { row =>
          val accumulator = Vector.newBuilder[Int]
          var currentSliceIndex = row.indexOfSlice(query.head)
          while (currentSliceIndex != -1) {
            accumulator += currentSliceIndex
            currentSliceIndex = row.indexOfSlice(query.head, currentSliceIndex + 1)
          }
          accumulator.result()
        }
        .zipWithIndex
        .flatMap(x => x._1.map(y => (y, x._2)))
        .filter { case (n, _) => n != -1 }
        .map { case (n, m) =>
          select(grid, query.head.length, query.length, n, m)
        }
        .contains(query)

      if (searchMatched) "YES" else "NO"
    }.foreach(println)
  }

  private def select(grid: Vector[Vector[Char]], n: Int, m: Int, nStart: Int, mStart: Int) = {
    grid.slice(mStart, mStart + m)
      .map(_.slice(nStart, nStart + n))
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