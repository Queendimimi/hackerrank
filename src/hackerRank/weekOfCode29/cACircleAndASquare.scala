package HackerRank.WeekOfCode29

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util
import java.util.InputMismatchException

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/17/2017
  */
object cACircleAndASquare {
  private val INPUT = ""

  def solve(): Unit = {
    val w = nextInt()
    val h = nextInt()
    val circleX = nextInt()
    val circleY = nextInt()
    val r = nextInt()
    val x1 = nextInt()
    val y1 = nextInt()
    val x3 = nextInt()
    val y3 = nextInt()
    val a = Canvas(w, h)
    val square = Vector((x1, y1), (x3, y3)).sortBy(_._1)
    a.drawCircle((circleX, circleY), r)
    a.drawSquare(square(0), square(1))
    out.println(a)
  }

  case class Pixel(x: Int, y: Int, var value: Boolean = false) {
    override def toString: String = {
      if (value) {
        "#"
      } else {
        "."
      }
    }
  }

  case class Square(a: (Double, Double), b: (Double, Double), c: (Double, Double), d: (Double, Double)) {

    def isInside(point: (Double, Double)): Boolean = {
      val AB = EuclideanVector(a, b)
      val AP = EuclideanVector(a, point)
      val BC = EuclideanVector(b, c)
      val BP = EuclideanVector(b, point)
      val ABdotAP = scalarProduct(AB, AP)
      val ABdotAB = scalarProduct(AB, AB)
      val BCdotBP = scalarProduct(BC, BP)
      val BCdotBC = scalarProduct(BC, BC)
      0 <= ABdotAP && ABdotAP <= ABdotAB && 0 <= BCdotBP && BCdotBP <= BCdotBC
    }
  }

  def scalarProduct(a: EuclideanVector, b: EuclideanVector): Double = {
    a.scalarProduct(b)
  }

  case class EuclideanVector(a: (Double, Double), b: (Double, Double)) {
    val x: Double = b._1 - a._1
    val y: Double = b._2 - a._2

    def scalarProduct(b: EuclideanVector): Double = {
      x * b.x + y * b.y
    }
  }

  case class Canvas(width: Int, height: Int) {
    private[this] val canvas = Array.tabulate[Pixel](width, height) {
      (i, j) => Pixel(i, j)
    }

    def setPixel(x: Int, y: Int): Unit = {
      canvas(x)(y).value = true
    }

    def drawCircle(center: (Int, Int), radius: Int): Unit = {
      for {
        x <- 0 until width
        y <- 0 until height
        if distanceSquared((x, y), center) <= radius * radius
      } yield setPixel(x, y)
    }

    def drawSquare(A: (Int, Int), C: (Int, Int)): Unit = {
      val B = ((A._1 + C._1 + C._2 - A._2) / 2.0, (A._2 + C._2 + A._1 - C._1) / 2.0)
      val D = ((A._1 + C._1 + A._2 - C._2) / 2.0, (A._2 + C._2 + C._1 - A._1) / 2.0)
      val square = Square((A._1.toDouble, A._2.toDouble), B, (C._1.toDouble, C._2.toDouble), D)
      for {
        x <- 0 until width
        y <- 0 until height
        if square.isInside((x.toDouble, y.toDouble))
      } yield setPixel(x, y)
    }

    def distanceSquared(a: (Int, Int), b: (Int, Int)): Int = {
      (a._1 - b._1) * (a._1 - b._1) + (a._2 - b._2) * (a._2 - b._2)
    }


    override def toString: String = {
      val result = StringBuilder.newBuilder
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          result.append(canvas(x)(y))
        }
        result.append("\n")
      }
      result.toString
    }
  }


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
    if (!INPUT.isEmpty) print(System.currentTimeMillis - s + "ms")
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

  private def nextDouble: Double = nextString.toDouble

  private def nextChar: Char = skip.toChar

  private def nextString: String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) { // when nextLine, (isSpaceChar(b) && b != ' ')
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
  }

  private def nextString(n: Int): Array[Char] = {
    val buf = new Array[Char](n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      buf({
        p += 1
        p - 1
      }) = b.toChar
      b = readByte()
    }
    if (n == p) buf else util.Arrays.copyOf(buf, p)
  }

  private def nextMultiLine(n: Int, m: Int): Array[Array[Char]] = {
    val map = new Array[Array[Char]](n)
    var i = 0
    while (i < n) {
      map(i) = nextString(m)
      i += 1
    }
    map
  }

  private def nextIntArray(n: Int): Array[Int] = {
    val a = new Array[Int](n)
    var i = 0
    while (i < n) {
      a(i) = nextInt()
      i += 1
      i - 1
    }
    a
  }

  private def nextLongArray(n: Int): Array[Long] = {
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = nextLong()
      i += 1
      i - 1
    }
    a
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

  private def print(o: AnyRef*): Unit = {
    System.out.println(java.util.Arrays.deepToString(o.toArray)
    )
  }
}