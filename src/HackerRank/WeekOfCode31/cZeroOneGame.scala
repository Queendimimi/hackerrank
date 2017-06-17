package HackerRank.WeekOfCode31

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/17/2017
  */
object cZeroOneGame {
  private val INPUT = ""

  def solve(): Unit = {
    val g = nextInt()
    var a0 = 0
    while (a0 < g) {
      val n = nextInt()
      val builder = ArrayBuffer.empty[Int]
      for (_ <- 0 until n) {
        builder += nextInt()
      }
      // If Alice wins, print 'Alice' on a new line; otherwise, print 'Bob'
      println(solve(builder))
      a0 += 1
    }
  }

  //  def isWin(game: String) = {
  //    if (game.length % 2 == 0) 0 else 1
  //  }
  //
  //  def solve(str: String) = {
  //    val games = str.split("11+").filter(_.nonEmpty)
  //    val result = games.view.map { game =>
  //      var result = game
  //      if (game.head == '1') result = result.drop(1)
  //      if (game.last == '1') result = result.dropRight(1)
  //      result
  //    }.filter(_.length > 2).map(isWin).fold(0)(_ ^ _)
  //    if (result == 0) "Bob" else "Alice"
  //  }

  def solve(input: mutable.Buffer[Int]): String = {
    if (input.length < 3) return "Bob"
    input.append(1, 1, 1)
    input.prepend(1, 1, 1)
    for (i <- 3 until input.length - 3) {
      if (input(i) == 1 && input(i - 1) == 0 && input(i + 1) == 0) {
        input(i) = 0
      }
      if (input(i) == 0 && input(i - 1) == 1 && input(i + 1) == 1 && input(i + 2) == 1
        && input(i - 2) == 1) {
        input(i) = 1
      }
      if (input(i) == 0 && input(i - 1) == 1 && input(i + 1) == 0 && input(i + 2) == 1
        && input(i - 2) == 1 && input(i + 3) == 1) {
        input(i) = 1
      }
      if (input(i) == 0 && input(i - 1) == 0 && input(i + 1) == 1 && input(i + 2) == 1
        && input(i - 2) == 1) {
        input(i) = 1
      }
    }
    if (input.count(_ == 0) % 2 == 0) "Bob" else "Alice"
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

  private def print(o: AnyRef*): Unit = {
    println(java.util.Arrays.deepToString(o.toArray))
  }
}