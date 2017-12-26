package HackerRank

import HackerRank.Graph.{Edge, Graph, Node, WeightedEdge}

object MinimumSpanningTree {

  private def kruskalMST[E <: Edge[N, E], N <: Node, Weight: Ordering, Sum]
  (graph: Graph[E, N], f: E => Weight, g: E => Sum, combine: (Sum, Sum) => Sum, empty: Sum) = {

    val unionFind = UnionFind(graph.numberOfNodes)
    val edgeList = graph.edges.sortBy(f)
    var sum = empty
    for (edge <- edgeList) {
      val u = graph.nodeId(edge.start).get
      val v = graph.nodeId(edge.end).get
      if (unionFind(u) != unionFind(v)) {
        unionFind.union(u, v)
        sum = combine(sum, g(edge))
      }
    }
    sum
  }

  implicit class MinimumSpanningTreeCost0[E <: Edge[N, E], N <: Node](val graph: Graph[E, N])
    extends AnyVal {
    def kruskalMinimumSpanningTreeCost[Weight: Numeric](f: E => Weight): Double = {
      kruskalMST[E, N, Weight, Double](graph, f, x => implicitly[Numeric[Weight]].toDouble(f(x)), _ + _, 0.0)
    }
  }

  implicit class MinimumSpanningTreeCost1[E <: WeightedEdge[N, E], N <: Node](val graph: Graph[E, N])
    extends AnyVal {
    def kruskalMinimumSpanningTreeCost(implicit ev: Numeric[E#Weight]): Double = {
      MinimumSpanningTreeCost0(graph).kruskalMinimumSpanningTreeCost[E#Weight](_.weight)
    }
  }

  implicit class MinimumSpanningTreeEdges0[E <: Edge[N, E], N <: Node](val graph: Graph[E, N])
    extends AnyVal {
    def kruskalMinimumSpanningTree[Weight: Numeric](f: E => Weight): List[E] = {
      kruskalMST[E, N, Weight, List[E]](graph, f, x => List(x), _ ::: _, List.empty[E])
    }
  }

  implicit class MinimumSpanningTreeEdges1[E <: WeightedEdge[N, E], N <: Node](val graph: Graph[E, N])
    extends AnyVal {
    def kruskalMinimumSpanningTree(implicit ev: Numeric[E#Weight]): List[E] = {
      MinimumSpanningTreeEdges0(graph).kruskalMinimumSpanningTree[E#Weight](_.weight)
    }
  }

}