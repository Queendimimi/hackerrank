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
    out.println(result.result().mkString("\n"))
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
  var in: java.io.InputStream = _
  var out: java.io.PrintWriter = _

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

  private def isSpaceChar(c: Int) = !(c >= 33 && c <= 126)

  private def skip = {
    var b = 0
    while ( {
      b = readByte()
      b != -1 && isSpaceChar(b)
    }) {}
    b
  }

  private def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
    }
    builder.result()
  }

  private def nextDouble[Coll[Double]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Double], Double, Coll[Double]]): Coll[Double] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextDouble()
    }
    builder.result()
  }

  private def nextChar[Coll[Char]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Char], Char, Coll[Char]]): Coll[Char] = {
    val builder = cbf()
    builder.sizeHint(n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      builder += b.toChar
      p += 1
      b = readByte()
    }
    builder.result()
  }

  private def nextMultiLine(n: Int, m: Int): Array[Array[Char]] = {
    val map = new Array[Array[Char]](n)
    var i = 0
    while (i < n) {
      map(i) = nextChar[Array](m)
      i += 1
    }
    map
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

  private def nextLong[Coll[Long]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Long], Long, Coll[Long]]): Coll[Long] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextLong()
    }
    builder.result()
  }

  private def nextDouble(): Double = nextString.toDouble

  private def nextChar: Char = skip.toChar

  private def nextString: String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) {
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
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

  private def nextLong(): Long = {
    var num = 0L
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
    throw new IOException("Read Long")
  }

  private def print(o: AnyRef*): Unit = {
    System.out.println(java.util.Arrays.deepToString(o.toArray)
    )
  }
}