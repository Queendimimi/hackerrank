package HackerRank.Training.DynamicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/15/2017
  */
private object TheLongestCommonSubsequence {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val m = nextInt()
    val n = nextInt()
    val a = nextInt[Vector](m)
    val b = nextInt[Vector](n)

    out.println(longestCommonSubsequence(a, b).mkString(" "))
  }

  private def longestCommonSubsequence(a: Vector[Int], b: Vector[Int]) = {
    lazy val lcsMemo: (Int, Int) ==> Int = Memo {
      case (m, n) if m == 0 || n == 0 => 0
      case (m, n) if a(m - 1) == b(n - 1) => 1 + lcsMemo(m - 1, n - 1)
      case (m, n) => Math.max(lcsMemo(m, n - 1), lcsMemo(m - 1, n))
    }

    def backTrack() = {
      val lcsBuilder = Vector.newBuilder[Int]
      var i = a.size
      var j = b.size
      while (i > 0 && j > 0) {
        if (a(i - 1) == b(j - 1)) {
          lcsBuilder += a(i - 1)
          i -= 1
          j -= 1
        }
        else {
          if (lcsMemo(i - 1, j) > lcsMemo(i, j - 1)) i -= 1 else j -= 1
        }
      }
      lcsBuilder.result().reverse
    }

    lcsMemo(a.size, b.size)

    backTrack()
  }

  final case class Memo[I, K, O](f: I => O)(implicit ev$1: I => K) extends (I => O) {
    type Input = I
    type Key = K
    type Output = O
    private val cache: mutable.Map[K, O] = mutable.Map.empty[K, O]

    override def apply(x: I): O = cache getOrElseUpdate(x, f(x))
  }

  type ==>[I, O] = Memo[I, I, O]

  //------------------------------------------------------------------------------------------//
  // Input-Output                                                                 
  //------------------------------------------------------------------------------------------//
  private var in: java.io.InputStream = _
  private var out: java.io.PrintWriter = _

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    run()
  }

  @throws[Exception]
  private def run(): Unit = {
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
  private var lenBuffer = 0
  private var ptrBuffer = 0

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
    out.println(java.util.Arrays.deepToString(o.toArray))
  }
}