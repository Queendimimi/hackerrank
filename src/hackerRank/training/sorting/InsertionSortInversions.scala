package HackerRank.Training.Sorting

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/23/2017
  */
object InsertionSortInversions {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()
    for (_ <- 1 to t) {
      val n = nextInt()
      println(countInversions(nextInt[List](n)))
    }
  }

  def countInversions(input: List[Int]): Long = {
    sortAndCountInversion(input)._2
  }

  def sortAndCountInversion(list: List[Int], count: Long = 0): (List[Int], Long) = {
    if (list.length <= 1) {
      (list, 0)
    } else {
      val (rightList, leftList) = list.splitAt(list.length / 2)
      val (sortedRightList, rightCount) = sortAndCountInversion(rightList, count)
      val (sortedLeftList, leftCount) = sortAndCountInversion(leftList, count)
      val (sortedMergedList, mergedCount) = merge(sortedRightList, sortedLeftList)
      (sortedMergedList, rightCount + leftCount + mergedCount)
    }
  }

  def merge(left: List[Int], right: List[Int], count: Long = 0): (List[Int], Long) = {
    (left, right) match {
      case (Nil, _) => (right, count)
      case (_, Nil) => (left, count)
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead > rightHead) {
          val (list, rightCount) = merge(left, rightTail, count)
          (rightHead :: list, count + left.length + rightCount)
        } else {
          val (list, leftCount) = merge(leftTail, right, count)
          (leftHead :: list, count + leftCount)
        }
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