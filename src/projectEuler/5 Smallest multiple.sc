def solve(largestDivisor: Int): Int = {
  var result = largestDivisor
  while ((1 to largestDivisor).exists(result % _ != 0)) {
    result = result + largestDivisor
  }
  result
}
