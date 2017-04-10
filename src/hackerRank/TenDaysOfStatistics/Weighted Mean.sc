def weightedMean(values: Vector[Int], weights: Vector[Int]) = {
  val nominator = values.zip(weights).map(pair => pair._1 * pair._2).sum
  val denominator = weights.sum
  nominator / denominator.toDouble
}

def round(input: Double): Double = {
  Math.round(input * 10.0) / 10.0
}

def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt()
  val values = Vector.newBuilder[Int]
  val weights = Vector.newBuilder[Int]
  for (i <- 0 until n) {
    values += sc.nextInt()
  }
  for (i <- 0 until n) {
    weights += sc.nextInt()
  }

  println(round(weightedMean(values.result, weights.result)))
}

val a = Vector(10, 40, 30, 50, 20, 10, 40, 30, 50, 20)
val b = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

println(round(weightedMean(a, b)))
