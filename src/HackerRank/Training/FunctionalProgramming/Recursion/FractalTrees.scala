package HackerRank.Training.FunctionalProgramming.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/16/2017
  */
private[this] object FractalTrees {
  private[this] val INPUT = "5"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    println(drawTree(100, 63, 32, nextInt()).reverse.map(_.mkString).mkString("\n"))
  }

  private[this] def drawTree(width: Int, height: Int, triangleHeight: Int, depth: Int) = {
    val board = Array.fill(height, width)('_')

    def recursiveDrawTree(startX: Int = if (width % 2 == 0) width / 2 - 1 else width / 2,
                          startY: Int = 0,
                          halfHeight: Int = triangleHeight / 2,
                          depth: Int = depth): Vector[Vector[Char]] = {
      if (depth > 0) {
        for (k <- 0 until halfHeight) {
          board(startY + k)(startX) = '1'
          board(startY + halfHeight + k)(startX + k + 1) = '1'
          board(startY + halfHeight + k)(startX - k - 1) = '1'
        }
        recursiveDrawTree(startX + halfHeight, startY + halfHeight * 2, halfHeight / 2, depth - 1)
        recursiveDrawTree(startX - halfHeight, startY + halfHeight * 2, halfHeight / 2, depth - 1)
      } else {
        board.map(_.toVector).toVector
      }
    }

    recursiveDrawTree()
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