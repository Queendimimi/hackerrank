def countCandy(n: Int, t: Int, outFlow: Seq[Int]): Int = {
  var restockedCandyCount = 0
  var candyLevel = n
  for (i <- 0 until t - 1) {
    candyLevel -= outFlow(i)
    if (candyLevel < 5) {
      restockedCandyCount += n - candyLevel
      candyLevel = n
    }
  }
  restockedCandyCount
}

val out = Array(3, 1, 7, 5)
println(countCandy(8, 4, out))