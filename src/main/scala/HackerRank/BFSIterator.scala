package HackerRank

import HackerRank.Graph.{Edge, Graph, Node}

import scala.collection.mutable

final case class BFSIterator[E <: Edge[N, E], N <: Node](graph: Graph[E, N], start: N)
  extends Iterator[(N, Int)] with Serializable {

  require(graph.contains(start))

  type Depth = Int
  private val visited = mutable.Map[N, Boolean]().withDefaultValue(false)
  private val q = new mutable.Queue[(N, Depth)]

  visited += start -> true
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

