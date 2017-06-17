import scala.collection.mutable

def query(left: Int, right: Int, x: Int, votes: Vector[Int]) = {
  val voteCount = mutable.Map.empty[Int, Int]
  //  val voteCount = mutable.SortedMap.empty[Int, Int]
  //    val voteCount = mutable.SortedMap.empty[Int, Int]
  votes.view.slice(left, right + 1).foreach(vote =>
    voteCount.get(vote)
      .fold[Unit](voteCount.put(vote, 1))(count => voteCount.update(vote, count + 1)))
  val votesWithX = voteCount.view.toVector.map(_.swap).filter(_._1 == x)
  if (votesWithX.nonEmpty) votesWithX.minBy(_._2)._2 else -1
}

def queryWithDecomposition(left: Int,
                           right: Int,
                           x: Int,
                           blockVoteCounts: Vector[Map[Int, Int]],
                           votes: Vector[Int]) = {
  val iLeft = left / 320
  val iRight = right / 320
  if (iLeft == iRight) {
    query(left, right, x, votes)
  } else {
    val resultVector = Vector.newBuilder[Map[Int, Int]]

    resultVector += countVotes(votes.slice(left, (iLeft + 1) * 320))
    for (i <- iLeft + 1 until iRight) {
      resultVector += blockVoteCounts(i)
    }
    resultVector += countVotes(votes.slice(iRight * 320, right + 1))

    val voteCount = resultVector.result().fold(Map.empty[Int, Int])(combine)
    val votesWithX = voteCount.view.toVector.map(_.swap).filter(_._1 == x)
    if (votesWithX.nonEmpty) votesWithX.minBy(_._2)._2 else -1
  }
}

def countVotes(votes: Vector[Int]) = {
  val voteCount = mutable.Map.empty[Int, Int]
  //  val voteCount = mutable.SortedMap.empty[Int, Int]
  //    val voteCount = mutable.SortedMap.empty[Int, Int]
  votes.foreach(vote =>
    voteCount.get(vote)
      .fold[Unit](voteCount.put(vote, 1))(count => voteCount.update(vote, count + 1)))
  voteCount.toMap
}

def combine(a: Map[Int, Int], b: Map[Int, Int]) = {
  a ++ b.map { case (k, v) => k -> (v + a.getOrElse(k, 0)) }
}


def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val t = sc.nextInt()
  var a0 = 0
  val result = Vector.newBuilder[Int]
  val blocks = Vector.newBuilder[Map[Int, Int]]
  var blockVoteCount = mutable.Map.empty[Int, Int]
  while (a0 < t) {
    val n = sc.nextInt()
    val v = Vector.newBuilder[Int]
    for (i <- 0 until n) {
      val vote = sc.nextInt()
      v += vote
      blockVoteCount.get(vote)
        .fold[Unit](blockVoteCount.put(vote, 1))(count => blockVoteCount.update(vote, count + 1))

      if ((i + 1) % 320 == 0 || i == n - 1) {
        blocks += blockVoteCount.toMap
        blockVoteCount = mutable.Map.empty[Int, Int]
      }
    }
    val g = sc.nextInt()
    val votes = v.result()
    val blockRes = blocks.result()
    var a1 = 0
    while (a1 < g) {
      val l = sc.nextInt()
      val r = sc.nextInt()
      val x = sc.nextInt()
      result += queryWithDecomposition(l, r, x, blockRes, votes)
      a1 += 1
    }
    a0 += 1
  }
  println(result.result().mkString("\n"))
}


def main2(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val t = sc.nextInt()
  var a0 = 0
  val result = Vector.newBuilder[Int]
  while (a0 < t) {
    val n = sc.nextInt()
    val v = Vector.newBuilder[Int]
    for (_ <- 0 until n) {
      v += sc.nextInt()
    }
    val g = sc.nextInt()
    val votes = v.result()
    var a1 = 0
    while (a1 < g) {
      val l = sc.nextInt()
      val r = sc.nextInt()
      val x = sc.nextInt()
      result += query(l, r, x, votes)
      a1 += 1
    }
    a0 += 1
  }
  println(result.result().mkString("\n"))
}

//
val test = Vector(4, 3, 0, 0, 0)

println(query(0, 1, 1, test))