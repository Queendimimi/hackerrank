import java.math.{RoundingMode, BigDecimal => BigDec}

val scale = 50

// Gauss-Legendre Algorithm
def pi(scale: Int): BigDecimal = {
  var a = BigDecimal.exact(1).underlying()
  var b = BigDecimal.exact(1).underlying().divide(
    sqrt(BigDecimal.exact(2), scale).underlying(), scale, RoundingMode.HALF_UP
  )
  var t = BigDecimal.exact(0.25).underlying()
  var x = BigDecimal.exact(1).underlying()
  var y: BigDec = null

  while (!a.equals(b)) {
    y = a
    a = a.add(b).divide(BigDecimal.exact(2).underlying(), scale, RoundingMode.HALF_UP)
    b = sqrt(BigDecimal(b.multiply(y)), scale).underlying()
    t = t.subtract(x.multiply(y.subtract(a).multiply(y.subtract(a))))
    x = x.multiply(BigDecimal.exact(2).underlying())
  }
  a.add(b).multiply(a.add(b)).divide(
    t.multiply(BigDecimal.exact(4).underlying()), scale, RoundingMode.HALF_UP)
}

// Babylonian square root
def sqrt(x: BigDecimal, scale: Integer): BigDecimal = {
  var n = BigDecimal.exact(0).underlying()
  var guess = BigDecimal(Math.sqrt(x.doubleValue())).underlying()

  while (!n.equals(guess)) {
    n = guess
    guess = x.underlying().divide(n, scale, RoundingMode.HALF_UP)
    guess = guess.add(n)
    guess = guess.divide(new BigDec(2), scale, RoundingMode.HALF_UP)
  }
  BigDecimal(guess)
}

val PI = pi(50)

def checkDenominator(denominator: Long): ((Long, Long), BigDecimal) = {
  val lowerBound = (PI * BigDecimal(denominator)).toLong
  val upperBound = lowerBound + 1
  val lowerBoundDistance = (PI - BigDecimal(lowerBound) / BigDecimal(denominator)).abs
  val upperBoundDistance = (BigDecimal(upperBound) / BigDecimal(denominator) - PI).abs
  if (upperBoundDistance < lowerBoundDistance) {
    ((upperBound, denominator), upperBoundDistance)
  } else {
    ((lowerBound, denominator), lowerBoundDistance)
  }
}

def closestFractionToPi(lowerDenominatorBound: Long, upperDenominatorBound: Long): (Long, Long) = {
  var result = ((0L, 0L), BigDecimal(Double.MaxValue))
  for (i <- upperDenominatorBound to lowerDenominatorBound by -1) {
    val currentCheck = checkDenominator(i)
    if (currentCheck._2 < result._2) {
      result = currentCheck
    }
  }
  result._1
}

val result = closestFractionToPi(1, 100000)
println(result._1 + "/" + result._2)

case class ResultState(left: (Long, Long),
                       approximation: (Long, Long),
                       right: (Long, Long))

def fraction(a: Long, b: Long): BigDecimal = {
  BigDecimal.exact(a).underlying().divide(BigDecimal.exact(b).underlying(), scale, RoundingMode.HALF_UP)
}

def fraction(a: (Long, Long)): BigDecimal = {
  BigDecimal.exact(a._1).underlying().divide(BigDecimal.exact(a._2).underlying(), scale, RoundingMode
    .HALF_UP)
}

def mediant(a: (Long, Long), b: (Long, Long)): (Long, Long) = {
  (a._1 + b._1, a._2 + b._2)
}

def approximate(maxDenominator: Long, target: BigDecimal, resultState: ResultState): ResultState = {
  println(resultState.approximation)
  if (resultState.approximation._2 > maxDenominator) {
    resultState
  }
  // left in Stern-Brocot Tree
  else if (target < fraction(resultState.approximation)) {
    val newApproximation = mediant(resultState.left, resultState.right)
    val newRight = resultState.approximation
    approximate(maxDenominator, target, ResultState(resultState.left, newApproximation, newRight))
  }
  // right in Stern-Brocot Tree
  else if (target > fraction(resultState.approximation)) {
    val newApproximation = mediant(resultState.left, resultState.right)
    val newLeft = resultState.approximation
    approximate(maxDenominator, target, ResultState(newLeft, newApproximation, resultState.right))
  } else throw new RuntimeException("Target is rational, or not precise enough")
}


def approximatePI(maxDenominator: Long): (Long, Long) = {
  approximate(maxDenominator, PI, ResultState((1, 1), (2, 1), (1, 0))).approximation
}