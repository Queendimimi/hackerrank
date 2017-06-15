package HackerRank.Training.Recursion

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/15/2017
  */
private object ThePowerSum {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val x = nextInt()
    val n = nextInt()

    val bases = (1 to nthRoot(n, x).toInt).map(x => power(x, n).toInt).toVector

    println(countWays(x, bases)(x))
  }

  private def nthRoot(n: Int, a: Double): Double = {
    @tailrec
    def loop(x0: Double): Double = {
      val x1 = 1.0d / n * ((n - 1) * x0 + a / power(x0, n - 1))
      if (x0 <= x1) {
        x0.toDouble
      } else {
        loop(x1)
      }
    }

    lazy val nthRootMemo: Double ==> Double = Memo {
      x => loop(x)
    }

    nthRootMemo(a / 2)
  }

  private def power(n: Double, i: Int): Double = {
    @tailrec
    def loop(n: Double, i: Int, current: Double): Double = {
      if (i == 1) {
        current
      } else {
        loop(n, i - 1, current * n)
      }
    }

    lazy val powerMemo: (Double, Int, Double) ==> Double = Memo {
      case (base, p, c) => loop(base, p, c)
    }

    if (i == 0) 1 else powerMemo(n, i, n)
  }

  private def countWays(amount: Int, coins: Vector[Int]) = {
    val cache = Array.fill[Int](amount + 1)(0)
    cache(0) = 1

    for {i <- coins
         j <- amount - i to 0 by -1} {
      cache(j + i) += cache(j)
    }

    cache.toVector
  }

  type ==>[I, O] = Memo[I, I, O]

  final case class Memo[I, K, O](f: I => O)(implicit ev$1: I => K) extends (I => O) {
    type Input = I
    type Key = K
    type Output = O
    private val cache: mutable.Map[K, O] = mutable.Map.empty[K, O]

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
  private def run(): Unit = {
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