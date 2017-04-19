import scala.collection.generic.CanBuildFrom
import scala.collection.{generic, mutable}


val MAX_N = 100001



var edgeList = mutable.ArrayBuffer.empty[Edge]

def calcWeight(a: Int, b: Int) = {
  a + b
}


case class UnionFind(size: Int, lazyConstruct: Boolean = false)
  extends PartialFunction[Int, Int] with generic.Growable[Int] {
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
    for (i <- 0 until size) {
      +=(i)
    }
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


trait Edge {
  def u: Int

  def v: Int

  def weight: Double
}

implicit class IterableExt[A, Coll](xs: Coll)
                                   (implicit c2s: Coll => Seq[A],
                                    cbf: CanBuildFrom[Coll, A, Coll]) {
  def rotateRight(i: Int): Coll = {
    val builder = cbf()
    val size = xs.size
    builder ++= xs.view.drop(size - (i % size)) ++ xs.view.take(size - (i % size))
    builder.result()
  }

  def rotateLeft(i: Int): Coll = {
    val builder = cbf()
    val size = xs.size
    builder ++= xs.view.drop(i % size) ++ xs.view.take(i % size)
    builder.result()
  }
}

private def nextChar[Coll[Char]]
(n: Int)(implicit cbf: CanBuildFrom[Coll[Char], Char, Coll[Char]]): Coll[Char] = ???

def kruskalMST[T, Coll](edges: Coll, unionFind: UnionFind)
                       (implicit t2e: T => Edge,
                        c2s: Coll => Seq[T],
                        cbf: CanBuildFrom[Coll, T, Coll]) = {
  val edgeList = edges.sortBy(_.weight)
  val builder = cbf()
  for (edge <- edgeList) {
    val u = edge.u
    val v = edge.v
    if (unionFind(u) != unionFind(v)) {
      unionFind.union(u, v)
      builder += edge
    }
  }
  builder.result()
}


//def minSpanningTree = {
//  edgeList = edgeList.sortBy(_._1.weight)
//  val result = Vector.newBuilder[(Weight, Edge)]
//  for (edge <- edgeList) {
//    val u = edge._2.u
//    val v = edge._2.v
//    if (findSet(u) != findSet(v)) {
//      unionSets(u, v)
//      result += edge
//    }
//  }
//  result.result()
//}

def gcd(a: Int, b: Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}


//def solve(tree: Vector[(Weight, Edge)]) = {
//  tree.view.map(_._1).foldLeft((0, 0))(
//    (sum: (Int, Int), weight) => (sum._1 + weight.a, sum._2 + weight.b)
//  )
//}
//
//def toString(fraction: (Int, Int)) = {
//  val g = gcd(fraction._1, fraction._2)
//  val numerator: Int = fraction._1 / g
//  val denominator: Int = fraction._2 / g
//
//  numerator + "/" + denominator
//}
//
//def main(args: Array[String]) {
//  val sc = new Scanner(System.in)
//  val n = sc.nextInt
//  val m = sc.nextInt
//  var a0 = 0
//
//  for (i <- 0 until n) {
//    makeSet(i)
//  }
//
//  while (a0 < m) {
//    val u = sc.nextInt
//    val v = sc.nextInt
//    val a = sc.nextInt
//    val b = sc.nextInt
//    edgeList.append((Weight(calcWeight(a, b), a, b), Edge(u, v)))
//    a0 += 1
//  }
//
//  //  println(toString(solve(minSpanningTree)))
//}
