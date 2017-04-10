def solve(upperBound: Long): Long = {
  sumDivisibleBy(3, upperBound - 1) + sumDivisibleBy(5, upperBound - 1) - sumDivisibleBy(15, upperBound - 1)
}

def sumDivisibleBy(n: Long, upperBound: Long): Long = {
  //geometric series (N*(N+1)/2); here with mutliples of n -> N = (number of mutiples of n < upperBound) % n == 0
  n * (upperBound / n) * ((upperBound / n) + 1) >> 1
}