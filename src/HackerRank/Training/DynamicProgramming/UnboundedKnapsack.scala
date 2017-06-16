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
private object UnboundedKnapsack {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val t = nextInt()
    next[(Int, Vector[Int]), Vector]({
      val n = nextInt()
      val capacity = nextInt()
      (capacity, nextInt[Vector](n))
    }, t).foreach {
      case (capacity, items) => println(maxSum(capacity, items))
    }
  }

  private def maxSum(capacity: Int, itemWeights: Vector[Int]) = {
    //    lazy val boundKnapsack: (Int, Int) ==> Int = Memo {
    //      case (i, j) if i < 0 || j == 0 =>
    //        0
    //      case (i, j) if j < itemWeights(i) =>
    //        boundKnapsack(i - 1, j)
    //      case (i, j) =>
    //        Math.max(boundKnapsack(i - 1, j), itemWeights(i) + boundKnapsack(i - 1, j - itemWeights(i)))
    //    }
    //    boundKnapsack(itemWeights.size - 1, capacity)

    val dp = Array.fill[Int](capacity + 1)(0)

    for {
      i <- 0 to capacity
      j <- itemWeights.indices
    } {
      if (itemWeights(j) <= i) {
        dp(i) = Math.max(dp(i), dp(i - itemWeights(j)) + itemWeights(j))
      }
    }

    dp(capacity)
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
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
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
}