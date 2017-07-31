package HackerRank.Training.FunctionalProgramming.DynamicProgramming

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}
import scala.math.BigInt

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/13/2017
  */
object Fibonacci {
//  private[this] val INPUT = "1\n100"
    private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val t = nextInt()
    nextInt[Vector](t).foreach(x => println(fibonacci(x) % BigInt(100000007)))
  }

  private[this] def fibonacciForSuperHighNumbers(n: BigInt, mod: BigInt) = {
    val fibonacciMatrix: Matrix = Matrix(Vector.tabulate(2, 2)((row, col) =>
      if (row == 1 && col == 1) 0 else 1
    ))

    fibonacciMatrix.power(
      exponent = n,
      mod
    )(0)(1)
  }

  private[this] final case class Matrix(matrix: Vector[Vector[BigInt]]) {
    val m: Int = matrix.size
    val n: Int = matrix.map(_.size).head

    lazy val identity = Matrix(Vector.tabulate(m, n)((row, col) =>
      if (row == col) 1 else 0
    ))

    def row(i: Int): Vector[BigInt] = matrix(i)

    def column(j: Int): Vector[BigInt] = matrix.map(_ (j))

    def mod(c: BigInt): Matrix = {
      Matrix(matrix.map(_.map(_ % c)))
    }

    def *(b: Matrix): Matrix = {
      Matrix(Vector.tabulate(m, b.n)((i, j) =>
        (this.row(i), b.column(j)).zipped.map(_ * _).sum))
    }

    def power(exponent: BigInt, mod: BigInt, a: Matrix = this): Matrix = {
      if (exponent == 0) {
        identity
      } else if (exponent % 2 == 1) {
        (a * power(exponent - 1, mod)) mod mod
      } else {
        val d = power(exponent / 2, mod)
        (d * d) mod mod
      }
    }

    def apply(i: Int)(j: Int): BigInt = {
      matrix(i)(j)
    }
  }

  private[this] final object fibonacci extends Memo[BigInt, BigInt, BigInt, (BigInt, BigInt)] {

    //  F(2n-1) = F(n)^2 + F(n-1)^2
    //  F(2n) = (2F(n-1) + F(n))*F(n)
    def _fibonacci(n: BigInt): (BigInt, BigInt) = {
      if (n == 0) {
        cache.getOrElseUpdate(n, (0, 1))
      } else {
        val (a, b) = _fibonacci(n / 2)
        val c = (2 * b - a) * a
        val d = a * a + b * b
        if (n % 2 == 0) {
          cache.getOrElseUpdate(n, (c, d))
        } else {
          cache.getOrElseUpdate(n, (d, c + d))
        }
      }
    }

    override def apply(v1: BigInt): BigInt = {
      _fibonacci(v1)._1
    }

  }

  abstract class Memo[I, K, O, M](implicit ev$1: I => K) extends (I => O) {
    private[this] type Input = I
    private[this] type Output = O
    private[this] type Memory = M
    protected val cache: mutable.Map[K, M] = mutable.Map.empty[K, M]

    override def apply(v1: I): O
  }

  type ==>[I, O] = Memo[I, I, O, O]

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
    }) {
    }
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
}