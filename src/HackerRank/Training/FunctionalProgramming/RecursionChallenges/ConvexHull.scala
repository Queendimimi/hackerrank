package HackerRank.Training.FunctionalProgramming.RecursionChallenges


import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/12/2017
  */
object ConvexHull {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val points = next[Point, Vector](Point(nextInt(), nextInt()), n)
    val convexHullPoints = convexHull(points)
    val perimeter = (convexHullPoints :+ convexHullPoints.head)
      .sliding(2)
      .foldLeft(0.0) { case (sum, value) =>
        sum + value.head.distance(value.last)
      }
    out.println(perimeter)
  }

  final case class Point(x: Int, y: Int) {
    def <(b: Point): Boolean = {
      (x < b.x) || ((b.x >= x) && (y < b.y))
    }

    def distance(b: Point): Double = {
      Math.sqrt((x - b.x) * (x - b.x) + (y - b.y) * (y - b.y))
    }
  }

  private def clockWise(a: Point, b: Point, c: Point) = {
    crossProduct(a, b, c) < 0
  }

  private def counterClockWise(a: Point, b: Point, c: Point) = {
    crossProduct(a, b, c) > 0
  }

  private def crossProduct(a: Point, b: Point, c: Point) = {
    a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)
  }

  private def convexHull(input: Vector[Point]) = {
    if (input.size < 4) {
      input
    } else {
      val points = input.distinct.sortWith(_ < _)
      val upperHull = mutable.ArrayBuffer.empty[Point] += points.head
      val lowerHull = mutable.ArrayBuffer.empty[Point] += points.head

      for (point <- points.iterator.drop(1)) {
        computeHalfHull(point, points, clockWise, upperHull)
        computeHalfHull(point, points, counterClockWise, lowerHull)
      }

      val resultBuilder = Vector.newBuilder[Point]
      resultBuilder ++= upperHull.iterator.drop(1)
      resultBuilder ++= lowerHull.reverseIterator.drop(1)

      resultBuilder.result()
    }
  }

  private def computeHalfHull(currentPoint: Point,
                              points: Vector[Point],
                              orientation: (Point, Point, Point) => Boolean,
                              builder: mutable.ArrayBuffer[Point]) = {
    if (currentPoint == points(points.size - 1) ||
      orientation(points.head, currentPoint, points.last)) {

      while (builder.size >= 2 && !orientation(
        builder(builder.size - 2),
        builder(builder.size - 1),
        currentPoint)
      ) {
        builder.remove(builder.size - 1)
      }
      builder += currentPoint
    }
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
    }) {
    }
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
    System.out.println(java.util.Arrays.deepToString(o.toArray)
    )
  }
}