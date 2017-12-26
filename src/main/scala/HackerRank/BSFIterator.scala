package HackerRank

import HackerRank.Graph.{Edge, ImmutableGraph, Node}

import scala.collection.mutable

final case class BSFIterator[E <: Edge[N, E], N <: Node](graph: ImmutableGraph[E, N], start: N)
  extends Iterator[N] with Serializable {

  require(graph.contains(start))

  private val visited = Array.fill(graph.numberOfNodes)(false)
  private val q = new mutable.Queue[N]
  private var n = 0
  private var disconnectedNodeIterator: Option[Iterator[N]] = None

  visited(graph.nodeId(start).get) = true
  q.enqueue(start)

  private def bsf() = {
    val res = q.dequeue()
    n += 1
    for (i <- graph.adjacencyList(graph.nodeId(res).get)) {
      if (!visited(graph.nodeId(i._2.end).get)) {
        visited(graph.nodeId(i._2.end).get) = true
        q.enqueue(i._2.end)
      }
    }
    res
  }

  private def disconnectedIterator() = {
    n += 1
    disconnectedNodeIterator.fold {
      disconnectedNodeIterator = Some(
        visited
          .view
          .zipWithIndex
          .filter(_._1)
          .map(x => graph.invertedNodeMap(x._2))
          .toIterator)
      disconnectedNodeIterator.get.next()
    }(_.next())
  }

  override def hasNext: Boolean = {
    n < graph.numberOfNodes
  }

  override def next(): N = {
    if (q.nonEmpty) {
      bsf()
    } else {
      disconnectedIterator()
    }
  }
}

