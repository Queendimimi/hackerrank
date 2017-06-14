package HackerRank.WeekOfCode31

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util
import java.util.InputMismatchException

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/17/2017
  */
object bAccurateSorting {
  private val INPUT = ""

  def solve(): Unit = {
    val q = nextInt()
    var a0 = 0
    while (a0 < q) {
      val n = nextInt()
      val a = new Array[Int](n)
      for (i <- 0 until n) {
        a(i) = nextInt()
      }
      if (sortable(a)) println("Yes") else println("No")
      a0 += 1
    }
  }

  def sortable(array: Array[Int]): Boolean = {
    var max = array(0)
    for (i <- 1 until array.length) {
      val temp = array(i)
      if (max < array(i)) max = array(i)
      if (array(i - 1) > temp && swappable(array(i), array(i - 1))) {
        array(i) = array(i - 1)
        array(i - 1) = temp
      }
      if (max != array(i)) return false
    }
    true
  }

  def swappable(a: Int, b: Int): Boolean = {
    Math.abs(a - b) == 1
  }

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