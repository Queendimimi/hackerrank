import scala.io.Source

def shortTermMovingAvg(prices: Array[Int]) = {
  prices.sliding(60).map(_.sum / 60.0)
}

def longTermMovingAvg(prices: Array[Int]) = {
  prices.sliding(300).map(_.sum / 300.0)
}

def combine(shortTermMovingAvg: Iterator[Double], longTermMovingAvg: Iterator[Double]) = {
  shortTermMovingAvg.drop(240).zip(longTermMovingAvg).zipWithIndex.map(pair =>
    (pair._2 + 300, pair._1))
}

def isCrossOver(previous: (Double, Double), current: (Double, Double)) = {
  val (previousShortTerm, previousLongTerm) = previous
  val (currentShortTerm, currentLongTerm) = current
  if (previousShortTerm > previousLongTerm && currentShortTerm <= currentLongTerm) {
    true
  } else if (previousShortTerm < previousLongTerm && currentShortTerm >= currentLongTerm) {
    true
  } else if (previousShortTerm == previousLongTerm && currentShortTerm != currentLongTerm) {
    true
  } else {
    false
  }
}

def round(input: Double): Double = {
  Math.round(input * 100.0) / 100.0
}

def crossOver(shortTermMovingAvg: Iterator[Double], longTermMovingAvg: Iterator[Double]) = {
  val combined = combine(shortTermMovingAvg, longTermMovingAvg)

  combined.sliding(2).map(pair => (pair.head, pair.last)).filter {
    case (previous, current) =>
      isCrossOver(previous._2, current._2)
  }.map(_._2)
}

def solve(prices: Array[Int]) = {
  crossOver(shortTermMovingAvg(prices), longTermMovingAvg(prices)).map(pair =>
    (pair._1, f"${round(pair._2._1)}%1.2f", f"${round(pair._2._2)}%1.2f"))
}

def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt()
  val p = new Array[Int](n)
  for (p_i <- 0 until n) {
    p(p_i) = sc.nextInt()
  }
  println(solve(p).map(_.productIterator.mkString(" ")).mkString("\n"))
}


//val test = Source.fromURL("https://hr-testcases-us-east-1.s3.amazonaws.com/36303/input00.txt?AWSAccessKeyId=AKIAJAMR4KJHHUS76CYQ&Expires=1490978096&Signature=BPNmNTl36%2BqotWXuqfmBPGzWLeg%3D&response-content-type=text%2Fplain")
val test = Source.fromURL("https://hr-testcases-us-east-1.s3.amazonaws.com/36303/input10.txt?AWSAccessKeyId=AKIAJAMR4KJHHUS76CYQ&Expires=1490978712&Signature=F7BO1lVjzio0nGT8%2BA74jWvBs%2BM%3D&response-content-type=text%2Fplain")
val builder = Array.newBuilder[Int]
for (line <- test.getLines()) {
  line.split(" ").map(_.toInt).foreach(builder += _)
}

solve(builder.result().drop(1))


println(solve(builder.result().drop(1)).map(_.productIterator.mkString(" ")).mkString("\n"))