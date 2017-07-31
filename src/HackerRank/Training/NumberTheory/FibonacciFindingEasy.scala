package HackerRank.Training.NumberTheory

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.math.BigInt
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/28/2017
  */
private[this] object FibonacciFindingEasy {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {

    val fibonacciMatrix: Matrix = Matrix(Vector.tabulate(2, 2)((row, col) =>
      if (row == 1 && col == 1) 0 else 1
    ))

    val t = nextInt()
    next[(BigInt, BigInt, BigInt), Vector]((BigInt(nextLong()), BigInt(nextLong()), BigInt(nextLong())), t)
      .foreach { case (a, b, n) =>
        println((fibonacciMatrix.power(n, mod = BigInt(1000000007)) * startMatrix(a, b)) (1)(1) % BigInt(1000000007))
      }
  }

  private[this] def startMatrix(a: BigInt, b: BigInt): Matrix = {
    Matrix(Vector(Vector(a + b, b), Vector(b, a)))
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

    def power(exponent: BigInt, mod: BigInt): Matrix = {

      def _power(e: BigInt, c: BigInt, a: Matrix = this): TailRec[Matrix] = {
        if (e == 0) {
          done(identity)
        } else if (e % 2 == 1) {
          done((a * tailcall(_power(e - 1, c, a)).result) mod c)
        } else {
          val d = tailcall(_power(e / 2, c, a)).result
          done((d * d) mod c)
        }
      }

      _power(exponent, mod).result
    }

    def apply(i: Int)(j: Int): BigInt = {
      matrix(i)(j)
    }
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