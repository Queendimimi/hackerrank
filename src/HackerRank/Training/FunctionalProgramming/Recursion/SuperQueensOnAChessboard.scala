package HackerRank.Training.FunctionalProgramming.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/18/2017
  */
private object SuperQueensOnAChessboard {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    println(Board(n, n).countPlaceableSuperQueens(n))
  }

  final case class Board(m: Int, n: Int) {
    require(m > 0)
    require(n > 0)
    private val board = Vector.fill(m, n)(false)

    //only checks leftSide!
    private def canSuperQueenByPlaced(i: Int, j: Int, board: Seq[Seq[Boolean]] = board) = {
//      require(i >= 0)
//      require(j >= 0)
//      require(i < m)
//      require(j < n)

      @tailrec
      def rookLeft(j: Int = j): Boolean = {
        if (j < 0) true else if (board(i)(j)) false else rookLeft(j - 1)
      }

      @tailrec
      def upperLeftBishop(i: Int = i, j: Int = j): Boolean = {
        if (i < 0 || j < 0) true else if (board(i)(j)) false else upperLeftBishop(i - 1, j - 1)
      }

      @tailrec
      def lowerLeftBishop(i: Int = i, j: Int = j): Boolean = {
        if (i >= m || j < 0) true else if (board(i)(j)) false else lowerLeftBishop(i + 1, j - 1)
      }

      def leftKnight(i: Int = i, j: Int = j) = {
        (if (i > 0 && j > 1) !board(i - 1)(j - 2) else true) &&
          (if (i > 1 && j > 0) !board(i - 2)(j - 1) else true) &&
          (if (i < m - 1 && j > 1) !board(i + 1)(j - 2) else true) &&
          (if (i < m - 2 && j > 0) !board(i + 2)(j - 1) else true)
      }


      leftKnight() && rookLeft() && upperLeftBishop() && lowerLeftBishop()
    }

    def countPlaceableSuperQueens(n: Int): Int = {

      val mutableBoard = board.map(_.to[mutable.ArrayBuffer]).to[mutable.ArrayBuffer]
      var accumulator = 0

      def recursiveCountPlaceableSuperQueens(j: Int = 0): Boolean = {
        if (j == n) {
          accumulator += 1
          true
        } else {
          for (i <- 0 until m) {
            if (canSuperQueenByPlaced(i, j, mutableBoard)) {
              mutableBoard(i)(j) = true
              recursiveCountPlaceableSuperQueens(j + 1)
              mutableBoard(i)(j) = false
            }
          }
          false
        }
      }

      recursiveCountPlaceableSuperQueens()
      accumulator
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
