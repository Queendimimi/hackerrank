package HackerRank.WeekOfCode31

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, Growable}
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/18/2017
  */
object SpanningTreeFraction {

  import Reader._
  import Writer._

  private[this] val TEST_INPUT: Option[String] = None

  //------------------------------------------------------------------------------------------//
  // Solution
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val n = nextInt()
    val m = nextInt()
    val edges = next[Edge, Array]({
      val u = nextInt()
      val v = nextInt()
      val a = nextInt()
      val b = nextInt()
      Edge(u, v, a, b)
    }, m)

    val epsilon = 0.00001
    val min = 0
    val max = 10000001
    val optimalMSTSum = fractionalLinearOptimization(improveTarget(edges, n, _), epsilon, min, max)
    val divider = gcd(optimalMSTSum._1.toInt, optimalMSTSum._2.toInt)
    println(optimalMSTSum._1.toInt / divider + "/" + optimalMSTSum._2.toInt / divider)
  }

  private[this] def fractionalLinearOptimization(targetFunction: Double => Either[(Double, Double), (Double, Double)],
                                                 epsilon: Double,
                                                 min: Int,
                                                 max: Int) = {
    @tailrec
    def _binarySearch(prevFactor: Double, left: Double, right: Double): (Double, Double) = {
      val factor = (left + right) / 2.0
      val target = targetFunction(factor)
      if (Math.abs(prevFactor - factor) < epsilon) {
        target.fold(identity, identity)
      } else {
        target match {
          case Left(_) => _binarySearch(factor, left, factor)
          case Right(_) => _binarySearch(factor, factor, right)
        }
      }
    }

    _binarySearch(0.0, min, max)
  }

  private[this] def improveTarget(edges: Seq[Edge], n: Int, factor: Double) = {
    val (aSum, bSum) = kruskalMST(edges, n, factor)
    val improvement = aSum >= bSum * factor
    if (improvement) Right((aSum, bSum)) else Left((aSum, bSum))
  }

  private[this] def kruskalMST(edges: Seq[Edge], n: Int, factor: Double) = {
    val unionFind = UnionFind(n)
    val edgeList = edges.sortBy(_.weight(factor))

    var aSum = 0.0
    var bSum = 0.0
    for (edge <- edgeList) {
      val u = edge.u
      val v = edge.v
      if (unionFind(u) != unionFind(v)) {
        unionFind.union(u, v)
        aSum += edge.a
        bSum += edge.b
      }
    }
    (aSum, bSum)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  case class Edge(u: Int, v: Int, a: Int, b: Int) {
    def weight(factor: Double): Double = {
      -(a - b * factor)
    }
  }

  case class UnionFind(size: Int, lazyConstruct: Boolean = false)
    extends PartialFunction[Int, Int] with Growable[Int] {
    private[this] type Rank = Int
    private[this] type Node = Int
    private[this] type Root = Int

    private[this] var parent = new Array[Node](size)
    private[this] var rank = new Array[Rank](size)

    if (!lazyConstruct) {
      for (i <- 0 until size) {
        +=(i)
      }
    }

    override def apply(x: Node): Root = {
      find(x)
    }

    override def clear(): Unit = {
      parent = new Array[Node](size)
      rank = new Array[Rank](size)
    }

    override def isDefinedAt(x: Int): Boolean = x < parent.length && 0 <= x

    override def +=(v: Node): UnionFind.this.type = {
      parent(v) = v
      rank(v) = 0
      this
    }

    private[this] def find(v: Node): Root = {
      if (v == parent(v)) {
        v
      } else {
        parent(v) = find(parent(v))
        parent(v)
      }
    }

    def sets: Map[Int, Iterable[Int]] = {
      (0 until size).groupBy(find)
    }

    def normalUnion(a: Node, b: Node): Unit = {
      parent(find(a)) = find(b)
    }

    def randomizedUnion(a: Node, b: Node): Unit = {
      // Randomized linking is O(an) too: http://www.cis.upenn.edu/~sanjeev/papers/soda14_disjoint_set_union.pdf
      if (scala.util.Random.nextBoolean()) {
        parent(find(a)) = find(b)
      } else {
        parent(find(b)) = find(a)
      }
    }

    //Union with path compression by rank
    def union(a: Node, b: Node): Unit = {
      var aRoot = find(a)
      var bRoot = find(b)

      if (aRoot != bRoot) {
        if (rank(aRoot) < rank(bRoot)) {
          val temp = aRoot
          aRoot = bRoot
          bRoot = temp
        }
        parent(bRoot) = aRoot
        if (rank(aRoot) == rank(bRoot)) {
          rank(aRoot) = rank(aRoot) + 1
        }
      }
    }
  }

  //------------------------------------------------------------------------------------------//
  // Run
  //------------------------------------------------------------------------------------------//
  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val s = System.currentTimeMillis
    solve()
    flush()
    if (TEST_INPUT.isDefined) System.out.println(System.currentTimeMillis - s + "ms")
  }

  //------------------------------------------------------------------------------------------//
  // Input
  //------------------------------------------------------------------------------------------//
  private[this] final object Reader {

    private[this] implicit val in = TEST_INPUT.fold(System.in)(s => new ByteArrayInputStream(s.getBytes))

    def nextSeq[T, Coll[_]](reader: => Seq[T], n: Int)
                           (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder ++= reader
      }
      builder.result()
    }

    def next[T, Coll[_]](reader: => T, n: Int)
                        (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += reader
      }
      builder.result()
    }

    def nextWithIndex[T, Coll[_]](reader: => T, n: Int)
                                 (implicit cbf: CanBuildFrom[Coll[(T, Int)], (T, Int), Coll[(T, Int)]]): Coll[(T, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((reader, i))
      }
      builder.result()
    }

    def nextDouble[Coll[Double]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[Double], Double, Coll[Double]]): Coll[Double] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += nextDouble()
      }
      builder.result()
    }

    def nextDoubleWithIndex[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[(Double, Int)], (Double, Int), Coll[(Double, Int)]]): Coll[(Double, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((nextDouble(), i))
      }
      builder.result()
    }

    def nextChar[Coll[_]]
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

    def nextCharWithIndex[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[(Char, Int)], (Char, Int), Coll[(Char, Int)]]): Coll[(Char, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      var b = skip
      var p = 0
      while (p < n && !isSpaceChar(b)) {
        builder += ((b.toChar, p))
        p += 1
        b = readByte()
      }
      builder.result()
    }

    def nextInt[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += nextInt()
      }
      builder.result()
    }

    def nextIntWithIndex[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[(Int, Int)], (Int, Int), Coll[(Int, Int)]]): Coll[(Int, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((nextInt(), i))
      }
      builder.result()
    }

    def nextLong[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[Long], Long, Coll[Long]]): Coll[Long] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += nextLong()
      }
      builder.result()
    }

    def nextLongWithIndex[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[(Long, Int)], (Long, Int), Coll[(Long, Int)]]): Coll[(Long, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((nextLong(), i))
      }
      builder.result()
    }

    def nextString[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[String], String, Coll[String]]): Coll[String] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += nextString()
      }
      builder.result()
    }

    def nextStringWithIndex[Coll[_]]
    (n: Int)(implicit cbf: CanBuildFrom[Coll[(String, Int)], (String, Int), Coll[(String, Int)]]): Coll[(String, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((nextString(), i))
      }
      builder.result()
    }

    def nextMultiLine(n: Int, m: Int): Array[Array[Char]] = {
      val map = new Array[Array[Char]](n)
      var i = 0
      while (i < n) {
        map(i) = nextChar[Array](m)
        i += 1
      }
      map
    }

    def nextDouble(): Double = nextString().toDouble

    def nextChar(): Char = skip.toChar

    def nextString(): String = {
      var b = skip
      val sb = new java.lang.StringBuilder
      while (!isSpaceChar(b)) {
        sb.appendCodePoint(b)
        b = readByte()
      }
      sb.toString
    }

    def nextInt(): Int = {
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

    def nextLong(): Long = {
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

    private[this] def readByte()(implicit in: java.io.InputStream): Int = {
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

    private[this] def isSpaceChar(c: Int) = !(c >= 33 && c <= 126)

    private[this] def skip = {
      var b = 0
      while ( {
        b = readByte()
        b != -1 && isSpaceChar(b)
      }) {}
      b
    }
  }

  //------------------------------------------------------------------------------------------//
  // Output
  //------------------------------------------------------------------------------------------//
  private[this] final object Writer {

    private[this] val out = new PrintWriter(System.out)

    def flush(): Unit = out.flush()

    def println(x: Any): Unit = out.println(x)

    def print(x: Any): Unit = out.print(x)
  }

}