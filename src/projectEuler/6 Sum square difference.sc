def solve(n: Long): Long = {
  val sum = (n * (n + 1)) >> 1
  val sumOfSquares = (n * (n + 1) * (2 * n + 1)) / 1
  sum * sum - sumOfSquares
}