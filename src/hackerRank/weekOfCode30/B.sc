def displayMinCalls(n: Int): String = {
  if (n == 2) {
    "min(int, int)"
  } else {
    "min(int, " + displayMinCalls(n - 1) + ")"
  }
}

println(displayMinCalls(2))
println(displayMinCalls(3))
println(displayMinCalls(50))
