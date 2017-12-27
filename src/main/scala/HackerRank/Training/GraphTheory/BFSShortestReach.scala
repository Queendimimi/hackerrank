package HackerRank.Training.GraphTheory

import java.io.{ByteArrayInputStream, IOException, InputStream, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, Clearable, Growable, Shrinkable}
import scala.collection.{TraversableOnce, mutable}
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 12/27/2017
  */
private[this] object BFSShortestReach {

  import Reader._
  import Writer._

  private[this] val TEST_INPUT: Option[String] = None

  //------------------------------------------------------------------------------------------//
  // Solution
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val q = next[Int]()
    for (_ <- 1 to q) {
      val nodeCount = next[Int]()
      val edgeCount = next[Int]()
      val graph = new MutableGraph[DefaultEdge, DefaultNode]()
      graph ++= (0 until nodeCount).view.map(DefaultNode)
      graph ++= next[(Int, Int), Vector]((next[Int](), next[Int]()), edgeCount)
        .map(x => DefaultEdge(x._1, x._2))
      val start = next[Int]()
      val reachableCosts = graph.breadthFirst(start).get.map(x => (x._1.name, x._2 * 6)).toMap
      val costs = (1 to nodeCount)
        .collect { case i if i != start => reachableCosts.get(i).fold(-1)(x => x) }
      println(costs.mkString(" "))
    }
  }

  trait Graph[E <: Edge[N, E], N <: Node] extends Serializable {
    protected val adjacencyList: collection.Seq[scala.collection.Seq[(Int, E)]]
    protected val nodeMap: collection.Map[N, Int]

    protected def invertedNodeMap: scala.collection.Map[Int, N] = nodeMap.map(_.swap)

    def edges: Seq[E] = {
      adjacencyList.view.flatten.map(_._2).toVector
    }

    def nodes: Set[N] = {
      nodeMap.view.map(_._1).toSet
    }

    def breadthFirst(node: N): Option[Iterator[(N, BFSIterator[E, N]#Depth)]] = {
      nodeId(node).map { _ =>
        BFSIterator[E, N](this, node)
      }
    }

    def nodeId(node: N): Option[Int] = {
      nodeMap.get(node)
    }

    def numberOfNodes: Int = {
      nodeMap.size
    }

    def outgoingEdges(node: N): Seq[E] = {
      nodeMap.get(node)
        .map(adjacencyList)
        .map(_.map(_._2).toVector)
        .fold(Vector.empty[E])(a => a)
    }

    def incomingEdges(node: N): Seq[E] = {
      nodeMap.get(node)
        .map(idx => adjacencyList.view.flatten.filter(_._1 == idx).map(_._2).toVector)
        .fold(Vector.empty[E])(a => a)
    }

    def contains(node: N): Boolean = {
      nodeId(node).fold(false)(_ => true)
    }
  }

  final case class BFSIterator[E <: Edge[N, E], N <: Node](graph: Graph[E, N], start: N)
    extends Iterator[(N, Int)] with Serializable {

    require(graph.contains(start))

    type Depth = Int
    private val visited = mutable.Map[N, Boolean]().withDefaultValue(false)
    private val q = new mutable.Queue[(N, Depth)]

    visited(start) = true
    q.enqueue((start, 0))

    override def hasNext: Boolean = {
      q.nonEmpty
    }

    override def next(): (N, Depth) = {
      val res = q.dequeue()
      for (i <- graph.outgoingEdges(res._1)) {
        if (!visited(i.end)) {
          visited(i.end) = true
          q.enqueue((i.end, res._2 + 1))
        }
      }
      res
    }
  }

  final case class ImmutableGraph[E <: Edge[N, E], N <: Node]
  (override val adjacencyList: Seq[Seq[(Int, E)]],
   override val nodeMap: Map[N, Int])
    extends Graph[E, N] {
    override lazy val invertedNodeMap: Map[Int, N] = {
      nodeMap.map(_.swap)
    }
  }

  class MutableGraph[E <: Edge[N, E], N <: Node] extends Graph[E, N]
    with mutable.Builder[E, ImmutableGraph[E, N]]
    with Growable[E]
    with Shrinkable[E]
    with Clearable {

    override protected val adjacencyList: mutable.ArrayBuffer[mutable.ArrayBuffer[(Int, E)]] = {
      mutable.ArrayBuffer.empty
    }

    override protected val nodeMap: mutable.Map[N, Int] = {
      mutable.Map.empty
    }

    private def add(edge: E): MutableGraph.this.type = {
      if (!nodeMap.isDefinedAt(edge.start)) {
        nodeMap += ((edge.start, nodeMap.size))
        adjacencyList += mutable.ArrayBuffer.empty
      }

      if (!nodeMap.isDefinedAt(edge.end)) {
        nodeMap += ((edge.end, nodeMap.size))
        adjacencyList += mutable.ArrayBuffer.empty
      }

      adjacencyList(nodeMap(edge.start)) += ((nodeMap(edge.end), edge))
      this
    }

    override def +=(edge: E): MutableGraph.this.type = {
      edge match {
        case _ if edge.directed => this add edge
        case _ => this add edge; this add edge.reverse
      }
    }

    def +=(node: N): MutableGraph.this.type = {
      if (!nodeMap.isDefinedAt(node)) {
        nodeMap += ((node, nodeMap.size))
        adjacencyList += mutable.ArrayBuffer.empty
      }
      this
    }

    def ++=(xs: TraversableOnce[N]): this.type = {
      @tailrec def loop(xs: scala.collection.LinearSeq[N]): Unit = {
        if (xs.nonEmpty) {
          this += xs.head
          loop(xs.tail)
        }
      }

      xs match {
        case xs: scala.collection.LinearSeq[N] => loop(xs)
        case _ => xs foreach +=
      }
      this
    }

    override def clear(): Unit = {
      adjacencyList.foreach(_.clear())
      nodeMap.clear()
    }

    override def -=(edge: E): MutableGraph.this.type = {
      if (nodeMap.isDefinedAt(edge.start) && nodeMap.isDefinedAt(edge.end)) {
        adjacencyList(nodeMap(edge.start))
          .view
          .zipWithIndex
          .find(_._1._1 == nodeMap(edge.end))
          .map(_._2)
          .foreach(adjacencyList(nodeMap(edge.start)).remove)
      }
      this
    }

    def -=(node: N): MutableGraph.this.type = {
      //directed
      nodeMap.get(node).foreach(nodeIdx => {
        // remove all incoming edges
        adjacencyList
          .view
          .zipWithIndex
          .filterNot(_._2 == nodeIdx)
          .map(_._1.zipWithIndex).foreach { adjL =>
          adjL
            .view
            .filter(_._1._1 == nodeMap(node))
            .map(_._2)
            .foreach(adjL.remove)
        }

        // remove all outgoing edges
        adjacencyList.remove(nodeIdx)

        //remove node
        nodeMap.remove(node)
      })
      this
    }

    override def result(): ImmutableGraph[E, N] = {
      ImmutableGraph(adjacencyList.view.map(_.toVector).toVector, nodeMap.toMap)
    }
  }

  trait Edge[N <: Node, T <: Edge[N, T]] extends Serializable {
    self: T =>
    def directed: Boolean

    def start: N

    def end: N

    def reverse: T

    override def toString: String = {
      if (directed) {
        s"$start ---> $end"
      } else {
        s"$start <--> $end"
      }
    }
  }

  trait WeightedEdge[N <: Node, T <: Edge[N, T]] extends Edge[N, T] {
    self: T =>

    type Weight

    def weight: Weight

    override def toString: String = {
      if (directed) {
        s"$start -- $weight -> $end"
      } else {
        s"$start <- $weight -> $end"
      }
    }
  }

  final case class DefaultEdge(start: DefaultNode,
                               end: DefaultNode,
                               directed: Boolean = false)
    extends Edge[DefaultNode, DefaultEdge] {
    override def reverse: DefaultEdge = DefaultEdge(end, start, directed)
  }

  trait Node extends Serializable

  implicit def intIsDefaultNode(v: Int): DefaultNode = {
    DefaultNode(v)
  }


  final case class DefaultNode(name: Int) extends Node {
    override def toString: String = name.toString
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

    private[this] implicit val in: InputStream = TEST_INPUT.fold(System.in)(s => new ByteArrayInputStream(s.getBytes))

    def next[T: ClassTag](): T = {
      implicitly[ClassTag[T]].runtimeClass match {
        case java.lang.Integer.TYPE => nextInt().asInstanceOf[T]
        case java.lang.Long.TYPE => nextLong().asInstanceOf[T]
        case java.lang.Double.TYPE => nextDouble().asInstanceOf[T]
        case java.lang.Character.TYPE => nextChar().asInstanceOf[T]
        case s if Class.forName("java.lang.String") == s => nextString().asInstanceOf[T]
        case b if Class.forName("scala.math.BigInt") == b => BigInt(nextString()).asInstanceOf[T]
        case b if Class.forName("scala.math.BigDecimal") == b => BigDecimal(nextString()).asInstanceOf[T]
        case _ => throw new RuntimeException("Unsupported input type.")
      }
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

    def next[T: ClassTag, Coll[_]](n: Int)
                                  (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += next[T]()
      }
      builder.result()
    }

    def nextWithIndex[T: ClassTag, Coll[_]](n: Int)
                                           (implicit cbf: CanBuildFrom[Coll[(T, Int)], (T, Int), Coll[(T, Int)]]): Coll[(T, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((next[T](), i))
      }
      builder.result()
    }

    def nextMultiLine[T: ClassTag](n: Int, m: Int): Seq[Seq[T]] = {
      val map = Vector.newBuilder[Vector[T]]
      var i = 0
      while (i < n) {
        map += next[T, Vector](m)
        i += 1
      }
      map.result()
    }

    private[this] def nextDouble(): Double = nextString().toDouble

    private[this] def nextChar(): Char = skip.toChar

    private[this] def nextString(): String = {
      var b = skip
      val sb = new java.lang.StringBuilder
      while (!isSpaceChar(b)) {
        sb.appendCodePoint(b)
        b = readByte()
      }
      sb.toString
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