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
private[this] object ThePowerSum {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val x = nextInt()
    val n = nextInt()

    val bases = (1 to nthRoot(n, x).toInt).map(x => power(x, n).toInt).toVector

    println(countWays(x, bases)(x))
  }

  private[this] def nthRoot(n: Int, a: Double): Double = {
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

  private[this] def power(n: Double, i: Int): Double = {
    @tailrec
    def loop(n: Double, i: Int, current: Double): Double = {
      if (i == 1) {
        current
      } else {
        loop(n, i - 1, current * n)
      }
    }

    lazy val powerMemo: (Double, Int, Double) ==> Double = Memo {
      case (_, p, c) if p == 1 => c
      case (base, p, c) => powerMemo(base, p - 1, c * base)
    }

    if (i == 0) 1 else powerMemo(n, i, n)
  }

  //bounded by 1 coin Change
  private[this] def countWays(amount: Int, coins: Vector[Int]) = {
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
    private[this] val cache: mutable.Map[K, O] = mutable.Map.empty[K, O]

    override def apply(x: I): O = cache getOrElseUpdate(x, f(x))
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
  private[this] def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
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

  private[this] val inputBuffer = new Array[Byte](1024)
  private[this] var lenBuffer = 0
  private[this] var ptrBuffer = 0

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
}