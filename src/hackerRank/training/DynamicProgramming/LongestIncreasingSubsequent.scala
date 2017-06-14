package HackerRank.Training.DynamicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/29/2017
  */
object LongestIncreasingSubsequent {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val input = nextInt[Array](n)
    val tailIndices = new Array[Int](n)
    tailIndices(0) = input(0)
    var length = 1
    for (i <- 1 until n) {
      if (tailIndices(0) > input(i)) {
        // new smallest
        tailIndices(0) = input(i)
      } else if (tailIndices(length - 1) < input(i)) {
        // input(i) extends longest sub sequence
        tailIndices(length) = input(i)
        length += 1
      } else {
        // input(i) is potential candidate in later sub sequence
        tailIndices(binarySearch(tailIndices, input(i), length) + 1) = input(i)
        //        tailIndices(binarySearch(tailIndices, input(i), 0, length - 1) + 1) = input(i)
      }
    }
    println(length)
  }

  private def binarySearch(coll: Seq[Int], target: Int, len: Int): Int = {
    var right = len - 1
    var left = 0
    var mid = 0
    var result = -1
    while (left <= right) {
      mid = left + (right - left - 1) / 2
      if (coll(mid) < target) {
        left = mid + 1
        result = mid
      } else if (coll(mid) == target) {
        return len - 1
      } else {
        right = mid - 1
      }
    }
    result
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

  private def nextInt[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
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