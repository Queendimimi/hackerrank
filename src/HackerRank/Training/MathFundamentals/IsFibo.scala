package HackerRank.Training.MathFundamentals

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.math.BigInt
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/28/2017
  */
private[this] object IsFibo {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val maxN = BigInt("10000000000")

    val fibonacciNumbers = Stream.from(0)
      .map(n =>  new Fibonacci[Long].apply(n))
      .takeWhile(_ <= maxN)
      .foldLeft(Set.newBuilder[Long])(_ += _).result()

    val t = nextInt()
    next[Long, Vector](nextLong(), t).foreach { n =>
      println(
        if (fibonacciNumbers.contains(n)) "IsFibo" else "IsNotFibo"
      )
    }
  }

  private[this] final class Fibonacci[T: Integral] extends (T ==> (T, T)) {

    import Integral.Implicits._

    //  F(2n-1) = F(n)^2 + F(n-1)^2
    //  F(2n) = (2F(n-1) + F(n))*F(n)
    def _fibonacci(n: T): TailRec[(T, T)] = {
      if (n == 0) {
        done(cache.getOrElseUpdate(n, (implicitly[Integral[T]].zero, implicitly[Integral[T]].one)))
      } else {
        val (a, b) = tailcall(_fibonacci(n / implicitly[Integral[T]].fromInt(2))).result
        val c = (implicitly[Integral[T]].fromInt(2) * b - a) * a
        val d = a * a + b * b
        if (n % implicitly[Integral[T]].fromInt(2) == 0) {
          done(cache.getOrElseUpdate(n, (c, d)))
        } else {
          done(cache.getOrElseUpdate(n, (d, c + d)))
        }
      }
    }

    override def apply(v1: T): T = {
      _fibonacci(v1).result._1
    }
  }

  trait Memo[I, K, O, M] extends (I => O) {
    private[this] type Input = I
    private[this] type Key = K
    private[this] type Output = O
    private[this] type Memory = M
    val cache: mutable.Map[K, M] = mutable.Map.empty[K, M]

    override def apply(v1: I): O
  }

  type ==>[I, M] = Memo[I, I, I, M]


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

  private[this] def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
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

  private[this] def nextLong(): Long = {
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