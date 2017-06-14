package HackerRank.WeekOfCode31

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/17/2017
  */
object fNominatingGroupLeaders {
  private val INPUT = ""

  def solve(): Unit = {
    val t = nextInt()
    var a0 = 0
    val result = Vector.newBuilder[Int]
    while (a0 < t) {
      val n = nextInt()
      //      val v = Vector.newBuilder[Int]
      //      for (_ <- 0 until n) {
      //        v += nextInt()
      //      }
      val v = nextInt[Vector](n)

      val g = nextInt()
      val votes = v
      var a1 = 0
      while (a1 < g) {
        val l = nextInt()
        val r = nextInt()
        val x = nextInt()
        result += query(l, r, x, votes)
        a1 += 1
      }
      a0 += 1
    }
    println(result.result().mkString("\n"))
  }

  private def query(left: Int, right: Int, x: Int, votes: Vector[Int]) = {
    val voteCount = mutable.Map.empty[Int, Int]
    votes.view.slice(left, right + 1).foreach(vote =>
      voteCount.get(vote)
        .fold[Unit](voteCount.put(vote, 1))(count => voteCount.update(vote, count + 1)))
    val votesWithX = voteCount.view.toVector.map(_.swap).filter(_._1 == x)
    if (votesWithX.nonEmpty) votesWithX.minBy(_._2)._2 else -1
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

  private def nextInt[Coll[Int]]
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

  private def print(o: AnyRef*): Unit = {
    println(java.util.Arrays.deepToString(o.toArray))
  }
}