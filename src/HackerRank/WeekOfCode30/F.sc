import scala.collection.mutable

def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt()
  val graph = Array.ofDim[Int](n, n)
  val vertices = new Array[(Int, Int)](n)
  for (g_i <- 0 until n) {
    var vertexDegree = 0
    for (g_j <- 0 until n) {
      graph(g_i)(g_j) = sc.nextInt()
      vertexDegree += graph(g_i)(g_j)
    }
    vertices(g_i) = (g_i, vertexDegree)
  }

  if (n < 3) {
    println("1 \n 1")
  }
  if (n == 3) {
    println("3 \n 1 2 3")
  }

  val verticesByDecreasingDegree = vertices.sortBy(-_._2).map(_._1)
  // your code goes here
  verticesByDecreasingDegree.foreach(println)
}

//noinspection AccessorLikeMethodIsUnit
def getTriangles(graph: Array[Array[Int]], verticesByDecreasingDegree: Array[Int]): Unit = {
  def degreeOf(vertex: Int) = {
    verticesByDecreasingDegree.indexOf(vertex)
  }

  def adjacentVertices(vertex: Int) = {
    graph(vertex).zipWithIndex.collect { case (1, index) => index }
  }

  val a = new mutable.HashMap[Int, mutable.Set[Int]]()
  for (i <- graph.indices) {
    a(i) = mutable.Set.empty[Int]
  }
  for (vertexA <- verticesByDecreasingDegree) {
    for (vertexB <- adjacentVertices(vertexA)) {
      if (degreeOf(vertexA) < degreeOf(vertexB)) {
        println(vertexA + "  " + vertexB)
        println(a(1))
        for (vertexC <- a(vertexA) intersect a(vertexB)) {
          println(vertexA + "  " + vertexB + "   " + vertexC)
        }
        a(vertexB) += vertexA
      }
    }
  }
}
def getTriangles(graph: Array[Array[Int]]) = {
  def adjacentVertices(vertex: Int) = {
    graph(vertex).zipWithIndex.collect { case (1, index) => index }
  }

  val result = mutable.Set.empty[Set[Int]]

  for (vertexA <- graph.indices) {
    for (vertexB <- adjacentVertices(vertexA)) {
      for (vertexC <- adjacentVertices(vertexB)) {
        if (graph(vertexC)(vertexA) == 1) {
          val triangle = Set[Int](vertexA, vertexB, vertexC)
          result += triangle
        }
      }
    }
  }
  result
}

def merge(triangles: mutable.Set[Set[Int]]): Set[Int] = {
  println(triangles)

  def internalMerge(trianglesB: mutable.Set[Set[Int]], lastSize: Int): Set[Int] = {
    val lSize = trianglesB.size
    if (trianglesB.size == lastSize) {
      trianglesB.head
    } else {
      val result = mutable.Set.empty[Set[Int]]
      for (triangleA <- trianglesB) {
        for (triangleB <- triangles) {
          val union = triangleA union triangleB
          if (union.size == triangleA.size + 1) {
            result += union
          }
        }
      }
      if (result.isEmpty) {
        internalMerge(trianglesB, lSize)
      } else {
        internalMerge(result, lSize)
      }
    }
  }

  internalMerge(triangles, 0)
}

def solve(graph: Array[Array[Int]]) = {
  merge(getTriangles(graph))
}


val tesG2 = Vector(
  Vector(0, 1, 1, 0, 0, 0),
  Vector(1, 0, 1, 1, 0, 0),
  Vector(1, 1, 0, 1, 0, 0),
  Vector(0, 1, 1, 0, 1, 1),
  Vector(0, 0, 0, 1, 0, 1),
  Vector(0, 0, 0, 1, 1, 0)).map(_.toArray).toArray


solve(tesG2)