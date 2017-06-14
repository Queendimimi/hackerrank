package HackerRank.Training.BasicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 5/23/2017
  */
object DayOfTheProgrammer {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    printDateInRussia(nextInt())
  }

  val normalYear = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  val leapYear = List(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def printDateInRussia(year: Int): Unit = {
    val date = dateInRussia(year, 256)
    println(
      ("%02d".format(date._1), "%02d".format(date._2), date._3).productIterator.mkString(".")
    )
  }

  def dateInRussia(year: Int, dayOfTheYear: Int): (Int, Int, Int) = {
    def daysAndMonth(months: List[Int]): (Int, Int) = {
      var dayOfPastMonth = 0
      var month = 0
      var i = 0
      while (dayOfPastMonth < dayOfTheYear) {
        dayOfPastMonth = dayOfPastMonth + months(i)
        month = month + 1
        i = i + 1
      }
      (dayOfPastMonth - months(i - 1), month)
    }

    if (year > 1918) {
      val dayAndMonth = daysAndMonth(gregorianYear(year))
      (dayOfTheYear - dayAndMonth._1, dayAndMonth._2, year)
    }
    else if (year < 1918) {
      val dayAndMonth = daysAndMonth(julianYear(year))
      (dayOfTheYear - dayAndMonth._1, dayAndMonth._2, year)
    } else {
      val dayAndMonth = daysAndMonth(gregorianYear(year).updated(1, 15))
      if (dayAndMonth._2 == 2) {
        (dayOfTheYear - dayAndMonth._1 + 13, dayAndMonth._2, year)
      } else {
        (dayOfTheYear - dayAndMonth._1, dayAndMonth._2, year)
      }
    }
  }

  def gregorianYear(year: Int): List[Int] = {
    if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) {
      leapYear
    } else {
      normalYear
    }
  }

  def julianYear(year: Int): List[Int] = {
    if (year % 4 == 0) {
      leapYear
    } else {
      normalYear
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
  def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) printCustom(System.currentTimeMillis - s + "ms")
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

  private def printCustom(o: AnyRef*): Unit = {
    println(java.util.Arrays.deepToString(o.toArray))
  }
}