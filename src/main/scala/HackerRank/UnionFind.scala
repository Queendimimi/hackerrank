package HackerRank

import scala.collection.generic.Growable

final case class UnionFind(size: Int, lazyConstruct: Boolean = false)
  extends PartialFunction[Int, Int] with Growable[Int] with Serializable {
  private[this] type Rank = Int
  private[this] type Node = Int
  private[this] type Root = Int

  private[this] var parent = new Array[Node](size)
  private[this] var rank = new Array[Rank](size)

  if (!lazyConstruct) {
    for (i <- 0 until size) {
      this += i
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