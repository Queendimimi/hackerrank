package HackerRank

import HackerRank.Graph.{Edge, Graph, Node}

import scala.collection.mutable

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 12/26/2017
  */
final case class DFSIterator[E <: Edge[N, E], N <: Node](graph: Graph[E, N], start: N)
  extends Iterator[(N, Int)] with Serializable {

  require(graph.contains(start))

  type Depth = Int
  private val visited = mutable.Map[N, Boolean]().withDefaultValue(false)
  private val stack = new mutable.ArrayStack[(N, Depth)]

  stack.push((start, 0))

  override def hasNext: Boolean = {
    stack.nonEmpty
  }

  override def next(): (N, Depth) = {
    if (!hasNext) throw new NoSuchElementException

    val res = stack.pop
    visited += res._1 -> true
    for (i <- graph.outgoingEdges(res._1)) {
      if (!visited(i.end)) {
        stack.push((i.end, res._2 + 1))
      }
    }
    res
  }
}