package HackerRank.ProjectEuler

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/15/2017
  */
private[this] object Euler031CoinSums {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val t = nextInt()
    val coins = Vector(1, 2, 5, 10, 20, 50, 100, 200)
    val amounts = nextInt[Vector](t)

    val lookup = countWays(coins, amounts.max)
    amounts.foreach(amount => println(lookup(amount) % BigInt(1000000007)))
  }

  private[this] def countWays(coins: Vector[Int], amount: Int) = {
    val cache = Array.fill[BigInt](amount + 1)(0)
    cache(0) = 1

    for {i <- coins
         j <- i to amount} {
      cache(j) += cache(j - i)
    }

    cache.toVector
  }

  lazy val _countWays: (Int, Vector[Int]) ==> BigInt = Memo {
    case (0, _) => 1
    case (amount, coins) if amount > 0 && coins.nonEmpty =>
      _countWays(amount - coins.head, coins) + _countWays(amount, coins.tail)
    case _ => 0
  }

  type ==>[I, O] = Memo[I, I, O]

  final case class Memo[I, K, O](f: I => O)(implicit ev$1: I => K) extends (I => O) {
    type Input = I
    type Key = K
    type Output = O
    val cache: mutable.Map[K, O] = mutable.Map.empty[K, O]

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

  private[this] def nextInt[Coll[_]]
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