package HackerRank.Training.FunctionalProgramming.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/18/2017
  */
private[this] object FractalsSierpinskiTriangles {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    println(drawTriangle(63, 32, nextInt()).reverse.map(_.mkString).mkString("\n"))
  }

  private[this] def drawTriangle(width: Int, height: Int, depth: Int) = {
    require(width % 2 != 0)
    require(height % 2 == 0)

    val builder = Vector.newBuilder[Array[Char]]
    for (row <- 0 until height) {
      val outer = Array.fill(row)('_')
      val inner = Array.fill(width - row * 2)('1')
      builder += outer ++ inner ++ outer
    }

    val board = builder.result()

    def recursiveDrawTriangle(startX: Int = width / 2,
                              startY: Int = 0,
                              width: Int = width / 2,
                              height: Int = height / 2,
                              depth: Int = depth): Vector[Vector[Char]] = {
      if (depth > 0) {
        for {k <- 0 until height
             w <- 0 to k} {
          board(startY + k)(startX + w) = '_'
          board(startY + k)(startX - w) = '_'
        }
        recursiveDrawTriangle(startX, startY + height, width / 2, height / 2, depth - 1)
        recursiveDrawTriangle(startX - width / 2 - 1, startY, width / 2, height / 2, depth - 1)
        recursiveDrawTriangle(startX + width / 2 + 1, startY, width / 2, height / 2, depth - 1)
      } else {
        board.map(_.toVector)
      }
    }

    recursiveDrawTriangle()
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

}