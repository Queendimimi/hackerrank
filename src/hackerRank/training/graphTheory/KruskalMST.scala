package HackerRank.Training.GraphTheory

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.{CanBuildFrom, Growable}
import scala.language.higherKinds

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/26/2017
  */
object KruskalMST {
  private val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val m = nextInt()
    val edges = nextSeq[Edge, Vector]({
      val u = nextInt() - 1
      val v = nextInt() - 1
      val weight = nextInt()
      Vector(Edge(u, v, weight), Edge(v, u, weight))
    }, m)
    println(kruskalMST(edges, UnionFind(n)))
  }

  case class Edge(u: Int, v: Int, weight: Double)

  def kruskalMST[T <: Edge, Coll](edges: Coll, unionFind: UnionFind)
                                 (implicit c2s: Coll => Seq[T],
                                  cbf: CanBuildFrom[Coll, T, Coll]): Int = {
    val edgeList = edges.sortBy(_.weight)
    var sum = 0.0
    for (edge <- edgeList) {
      val u = edge.u
      val v = edge.v
      if (unionFind(u) != unionFind(v)) {
        unionFind.union(u, v)
        sum += edge.weight
      }
    }
    sum.toInt
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
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
  }

  private def nextSeq[T, Coll[_]](reader: => Seq[T], n: Int)
                                 (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder ++= reader
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
}