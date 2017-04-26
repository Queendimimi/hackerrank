import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random


def kragerMinCut(graph: mutable.Map[Int, Array[Int]]): Int = {
  val mutableGraph = mutable.HashMap(graph.toSeq: _*).map {
    case (vertex, edges) => vertex -> edges.toBuffer
  }

  def mergeVertices(x: (Int, Int)) = {
    val (vertexA, vertexB) = x
    //    println(vertexA + "   " + vertexB)

    def removeSelfEdges() = {
      val edges = mutableGraph.getOrElse(vertexA, throw new RuntimeException)
      while (edges.contains(vertexA)) {
        edges -= vertexA
      }
    }

    val vertexAEdges = mutableGraph.getOrElse(vertexA, throw new RuntimeException)
    val vertexBEdges = mutableGraph.getOrElse(vertexB, throw new RuntimeException)
    val mergedEdges = vertexAEdges ++ vertexBEdges
    mutableGraph.remove(vertexB)
    mutableGraph.remove(vertexA)
    mutableGraph += vertexA -> mergedEdges
    mutableGraph.foreach { case (vertex, edges) =>
      while (edges.contains(vertexB)) {
        edges -= vertexB
        edges += vertexA
      }
    }
    removeSelfEdges()
  }

  def pickRandomEdge(): (Int, Int) = {
    val edges = mutableGraph.toVector.flatMap {
      case (vertex, edgeList) => edgeList.map((vertex, _))
    }
    val rnd = new Random
    edges(rnd.nextInt(edges.length))
  }

  while (mutableGraph.size > 2) {
    mergeVertices(pickRandomEdge())
  }

  mutableGraph.head._2.size
}

def monteCarloMinCut(graph: mutable.Map[Int, Array[Int]]): Int = {
  var minCut = Integer.MAX_VALUE
  for (_ <- 1 to 1000) {
    val temp = kragerMinCut(graph)
    if(temp < minCut) {
      minCut = temp
    }
  }
  minCut
}

val graph = new mutable.HashMap[Int, Array[Int]] withDefaultValue Array.empty
val test = Source.fromURL("https://d3c33hcgiwev3.cloudfront.net/_f370cd8b4d3482c940e4a57f489a200b_kargerMinCut.txt?Expires=1489795200&Signature=DzNeHXxmm9bvCPTkoMLM~qgOjb~gf4vudE2R4lYnXk6ZFYKaEv5uhjeyafoWxoDXUHanpE8pT8m6f-GxY4aXObJ78iz1QzOSndrwRpdA7JTsqdPSQcc1FZzYU6ZmA2EdXcY4-ben5enOn1GD8ilu~0PnrFbaJkSxs~~qRH-VIb8_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A")
val builder = new ListBuffer[Int]()
for (line <- test.getLines) {
  val vertex = line.split("\\s+").map(_.toInt)
  graph += vertex(0) -> vertex.tail
}



monteCarloMinCut(graph)