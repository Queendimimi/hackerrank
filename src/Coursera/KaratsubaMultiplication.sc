def countDigits(x: BigInt, result: Integer = 1): Integer = {
  if (x < 10) {
    result
  } else {
    countDigits(x / 10, result + 1)
  }
}

def split(x: BigInt, multiplier: BigInt): (BigInt, BigInt) = {
  val b = x / multiplier
  (x - b * multiplier, b)
}

def karatsubaMultiplication(x: BigInt, y: BigInt): BigInt = {
  if (x < 10 || y < 10) {
    x * y
  } else {
    val m = Math.max(countDigits(x), countDigits(y))
    val n = m / 2 + m % 2
    val multiplier = BigInt(10).pow(n)
    val (a, b) = split(x, multiplier)
    val (c, d) = split(y, multiplier)
    val ac = karatsubaMultiplication(a, c)
    val bd = karatsubaMultiplication(b, d)
    val abcd = karatsubaMultiplication(a + b, c + d)
    ac + (abcd - ac - bd) * multiplier + bd * BigInt(10).pow(2 * n)
  }
}

println(karatsubaMultiplication(
  BigInt("3141592653589793238462643383279502884197169399375105820974944592"),
  BigInt("2718281828459045235360287471352662497757247093699959574966967627")
))
