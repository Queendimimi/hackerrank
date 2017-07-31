package HackerRank.Training.Search

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.Searching._
import scala.collection.generic.CanBuildFrom
import scala.io.Source
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/20/2017
  */
object CtciIceCreamParlor {
  //  private[this] val INPUT = ""
  private[this] val source = Source.fromURL("https://hr-testcases-us-east-1.s3.amazonaws.com/24108/input01.txt?AWSAccessKeyId=AKIAJAMR4KJHHUS76CYQ&Expires=1492682262&Signature=9OVgVTqrubx%2BTbFIpMFX9fCUYGw%3D&response-content-type=text%2Fplain")
  private[this] val INPUT = source.getLines mkString "\n"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val t = nextInt()
    for (_ <- 0 until t) {
      val money = nextInt()
      val n = nextInt()
      val flavorCosts = nextInt[Array](n)
      val flavors = findFlavors(flavorCosts.sorted, money).get
      val a = linearSearch(flavorCosts, flavors._1).get
      val b = linearSearch(flavorCosts, flavors._2, fromLeft = false).get
      if (a < b) {
        println(a + " " + b)
      } else {
        println(b + " " + a)
      }
    }
  }

  private[this] def findFlavors(flavorCosts: Seq[Int], money: Int) = {
    split(money).find { case (a, b) =>
      if (a != b) {
        flavorCosts.search(a).isInstanceOf[Found] && flavorCosts.search(b).isInstanceOf[Found]
      } else {
        if (flavorCosts.count(_ == a) >= 2) true else false
      }
    }
  }

  private[this] def split(n: Int) = {
    val builder = Array.newBuilder[(Int, Int)]
    for (i <- 1 to n / 2) {
      builder += ((i, n - i))
    }
    builder.result()
  }

  private[this] def linearSearch(coll: Seq[Int], elem: Int, fromLeft: Boolean = true): Option[Int] = {
    if (fromLeft) {
      for (i <- coll.indices) {
        if (coll(i) == elem) return Some(i + 1)
      }
      None
    } else {
      for (i <- coll.indices.reverse) {
        if (coll(i) == elem) return Some(i + 1)
      }
      None
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
  def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) print(System.currentTimeMillis - s + "ms")
  }

  private[this] val inputBuffer = new Array[Byte](1024)
  var lenBuffer = 0
  var ptrBuffer = 0

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

  private[this] def nextInt[Coll[Int]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
    }
    builder.result()
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

  private[this] def print(o: AnyRef*): Unit = {
    println(java.util.Arrays.deepToString(o.toArray))
  }
}