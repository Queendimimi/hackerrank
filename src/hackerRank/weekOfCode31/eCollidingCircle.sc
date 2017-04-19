
def solve(array: Array[Int], k: Int): Double = {
  if (array.length - 1 == k) {
    val sum = array.sum
    Math.PI * sum * sum
  } else if (k == 0) {
    array.map(i => i * i * Math.PI).sum / k
  } else {
    val factor = getNumberOfCombination(array.length, k)
    array.combinations(k + 1).map { combination =>
      val remainingCircles = array diff combination
      //      remainingCircles foreach println
      //      combination foreach println
      val res1 = remainingCircles.map(circle => Math.PI * circle * circle).sum
      val sum = combination.sum
      val res2 = sum * sum * Math.PI
      //      println(res1)
      //      println(res2)
      //      println(res1 + res2)
      res1 + res2
    }.sum / choose(array.length, k)
  }
}

def getNumberOfCombination(n: Int, k: Int) = {
  choose(n, 2) * multiplication(n, k)
}

def multiplication(n: Int, k: Int) = {
  Stream.from(n - 2, -1).take(k - 1).product
}

def choose(n: Int, k: Int): Long = {
  if (k == 0) 1L else (n * choose(n - 1, k - 1)) / k.toLong
}


def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt()
  val k = sc.nextInt()
  val r = new Array[Int](n)
  for (r_i <- 0 until n) {
    r(r_i) = sc.nextInt()
  }
  println(solve(r, k))
}


println(solve(Vector(1, 2, 3).toArray, 1))

println(multiplication(3, 1))

println(getNumberOfCombination(3, 1))
