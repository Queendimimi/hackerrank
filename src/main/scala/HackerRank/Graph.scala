package HackerRank

import scala.collection.generic.{Clearable, Growable, Shrinkable}
import scala.collection.mutable

object Graph {

  trait Graph[E <: Edge[N, E], N <: Node] extends Serializable {
    protected val adjacencyList: scala.collection.Seq[scala.collection.Seq[(Int, E)]]
    protected val nodeMap: scala.collection.Map[N, Int]

    protected def invertedNodeMap: scala.collection.Map[Int, N] = nodeMap.map(_.swap)

    def edges: Seq[E] = {
      adjacencyList.view.flatten.map(_._2).toVector
    }

    def nodes: Set[N] = {
      nodeMap.view.map(_._1).toSet
    }

    def breadthFirst(node: N): Option[Iterator[(N, BSFIterator[E, N]#Depth)]] = {
      nodeId(node).map { _ =>
        BSFIterator(this, node)
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

  final case class ImmutableGraph[E <: Edge[N, E], N <: Node]
  (override val adjacencyList: Seq[Seq[(Int, E)]],
   override val nodeMap: Map[N, Int])
    extends Graph[E, N] {
    override lazy val invertedNodeMap: Map[Int, N] = {
      nodeMap.map(_.swap)
    }
  }

  class DirectedMutableGraph[E <: Edge[N, E], N <: Node] extends MutableGraph[E, N] {
    override def -=(node: N): DirectedMutableGraph.this.type = {
      //undirected
      nodeMap.get(node).foreach(nodeIdx => {
        // remove all incoming edges
        adjacencyList(nodeIdx).view
          .foreach { case (idx, _) =>
            adjacencyList(idx)
              .view
              .zipWithIndex
              .find(_._1._1 == nodeIdx)
              .map(_._2)
              .foreach(adjacencyList(idx).remove)
          }
        // remove all outgoing edges
        adjacencyList.remove(nodeIdx)

        //remove node
        nodeMap.remove(node)
      })
      this
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
                               weight: Double,
                               directed: Boolean = true)
    extends WeightedEdge[DefaultNode, DefaultEdge] {
    override type Weight = Double

    override def reverse: DefaultEdge = DefaultEdge(end, start, weight, directed)
  }

  trait Node extends Serializable

  implicit def intIsDefaultNode(v: Int): DefaultNode = {
    DefaultNode(v)
  }


  final case class DefaultNode(name: Int) extends Node {
    override def toString: String = name.toString
  }

}