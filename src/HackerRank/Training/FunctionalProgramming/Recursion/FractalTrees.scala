package HackerRank.Training.FunctionalProgramming.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/16/2017
  */
private object FractalTrees {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val board = Array.fill(63, 100)('_')
    drawTree(board, 0, 49, depth = nextInt(), 16)
    println(board.reverse.map(_.mkString).mkString("\n"))
  }

  private def drawTree(board: Array[Array[Char]],
                       startY: Int,
                       startX: Int,
                       depth: Int,
                       halfHeight: Int) {
    if (depth > 0) {
      for (k <- 0 until halfHeight) {
        board(startY + k)(startX) = '1'
        board(startY + halfHeight + k)(startX + k + 1) = '1'
        board(startY + halfHeight + k)(startX - k - 1) = '1'
      }
      drawTree(board, startY + halfHeight * 2, startX + halfHeight, depth - 1, halfHeight / 2)
      drawTree(board, startY + halfHeight * 2, startX - halfHeight, depth - 1, halfHeight / 2)
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
}