package HackerRank.Training.BasicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 7/1/2017
  */
private[this] object TheTimeInWords {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    println(timeToWord(nextInt(), nextInt()))
  }

  private[this] def timeToWord(h: Int, m: Int): String = {
    val wordLookup = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six",
      7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen",
      14 -> "fourteen", 15 -> "quarter", 16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen",
      20 -> "twenty", 21 -> "twenty one", 22 -> "twenty two", 23 -> "twenty three", 24 -> "twenty four",
      25 -> "twenty five", 26 -> "twenty six", 27 -> "twenty seven", 28 -> "twenty eight", 29 -> "twenty nine",
      30 -> "half")

    (h, m) match {
      case (hours, minutes) if minutes == 0 =>
        wordLookup(hours) + " o' clock"

      case (hours, minutes) if minutes == 15 || minutes == 30 =>
        wordLookup(minutes) + " past " + wordLookup(hours)

      case (hours, minutes) if minutes < 30 =>
        wordLookup(minutes) + " minutes past " + wordLookup(hours)

      case (hours, minutes) if (60 - minutes) == 15 =>
        wordLookup(60 - minutes) + " to " + wordLookup(hours + 1)

      case (hours, minutes) =>
        wordLookup(60 - minutes) + " minutes to " + wordLookup(hours + 1)
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