package HackerRank.ProjectEuler

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/22/2017
  */
object Euler004LargestPalindromeProduct {
  private val INPUT = "2\n101110\n800000"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()
    val palindromes = generate3DigitPalindromes().sorted
    nextInt[Array](t).foreach(x => println(palindromes(
      binarySearch(palindromes, x, 0, palindromes.length))
    ))
  }

  @tailrec
  def binarySearch(coll: Seq[Int], target: Int, left: Int, right: Int): Int = {

    val idx = left + (right - left - 1) / 2
    if (coll(idx) >= target && coll(idx - 1) < target) {
      idx - 1
    } else {
      math.signum(Integer.compare(target, coll(idx))) match {
        case -1 => binarySearch(coll, target, left, idx)
        case 1 => binarySearch(coll, target, idx + 1, right)
        case _ => idx
      }
    }
  }

  private def generate3DigitPalindromes() = {
    val builder = Vector.newBuilder[Int]
    var prod: Int = 0
    var i: Int = 100
    while (i <= 999) {
      var j: Int = 100
      while (j <= 999) {
        prod = i * j
        if (isPalindrome(prod)) {
          builder += prod
        }
        j += 1
        j - 1
      }
      i += 1
      i - 1
    }
    builder.result()
  }

  private def isPalindrome(n: Int): Boolean = {
    val pair = n.toString.splitAt(3)
    pair._1 == pair._2.reverse
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