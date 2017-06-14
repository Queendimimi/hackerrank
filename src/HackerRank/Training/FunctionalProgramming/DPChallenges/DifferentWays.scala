package HackerRank.Training.FunctionalProgramming.DPChallenges

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/13/2017
  */
object DifferentWays {
  //  private val INPUT = "1\n10000 2000"
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()

    for (_ <- 0 until t) {
      println(choose(nextInt(), nextInt()) % BigInt(100000007))
    }
  }

  type ==>[I, O] = Memo[I, I, O]

  lazy val choose: (Int, Int) ==> BigInt = Memo {
    case (n, 0) =>
      require(0 <= n)
      1
    case (n, k) if k == n =>
      1
    case (n, k) if k > (n / 2) =>
      require(k <= n)
      choose(n, n - k)
    case (n, k) =>
      require(k <= n)
      n * choose(n - 1, k - 1) / k
  }

  case class Memo[I, K, O](f: I => O)(implicit ev$1: I => K) extends (I => O) {
    type Input = I
    type Key = K
    type Output = O
    val cache: mutable.Map[K, O] = mutable.Map.empty[K, O]

    override def apply(x: I): O = cache getOrElseUpdate(x, f(x))
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