def solve(number: Long) = {
  var n = number
  var result = 0L
  var divisor = 2L
  while ((divisor * divisor) <= n) {
    if (n % divisor == 0) {
      result = divisor
      n = n / divisor
    } else {
      divisor = divisor + 1
    }
  }
  if (n != 1) {
    result = n
  }
  result
}

println(solve(10))