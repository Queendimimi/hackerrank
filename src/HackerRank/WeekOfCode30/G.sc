def longestCommonSubString(a: String, b: String): Int = {
  val m = a.length
  val n = b.length
  val dp = Array.ofDim[Int](m + 1, n + 1)
  var result = 0

  for (i <- 0 to m; j <- 0 to n) {
    if (i == 0 || j == 0) {
      dp(i)(j) = 0
    } else if (a(i - 1) == b(j - 1)) {
      dp(i)(j) = dp(i - 1)(j - 1) + 1
      result = Integer.max(result, dp(i)(j))
    } else {
      dp(i)(j) = 0
    }
  }
  result
}





println(longestCommonSubString("probieren", "birkerem"))