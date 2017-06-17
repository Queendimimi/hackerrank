import java.math.{RoundingMode, BigDecimal => BigDec}

val scale = 50
val PI = pi

// Gauss-Legendre Algorithm
def pi: BigDecimal = {
  var a = BigDecimal.exact(1).underlying()
  var b = BigDecimal.exact(1).underlying().divide(
    sqrt(BigDecimal.exact(2)).underlying(), scale, RoundingMode.HALF_UP
  )
  var t = BigDecimal.exact(0.25).underlying()
  var x = BigDecimal.exact(1).underlying()
  var y: BigDec = null

  while (!a.equals(b)) {
    y = a
    a = a.add(b).divide(BigDecimal.exact(2).underlying(), scale, RoundingMode.HALF_UP)
    b = sqrt(BigDecimal(b.multiply(y))).underlying()
    t = t.subtract(x.multiply(y.subtract(a).multiply(y.subtract(a))))
    x = x.multiply(BigDecimal.exact(2).underlying())
  }
  a.add(b).multiply(a.add(b)).divide(
    t.multiply(BigDecimal.exact(4).underlying()), scale, RoundingMode.HALF_UP)
}

// Babylonian square root
def sqrt(x: BigDecimal): BigDecimal = {
  var n = BigDecimal.exact(0).underlying()
  var guess = BigDecimal(Math.sqrt(x.doubleValue())).underlying()

  while (!n.equals(guess)) {
    n = guess
    guess = x.underlying().divide(n, scale, RoundingMode.HALF_UP)
    guess = guess.add(n)
    guess = guess.divide(new BigDec(2), scale, RoundingMode.HALF_UP)
  }
  guess
}

case class ResultState(left: (Long, Long),
                       approximation: (Long, Long),
                       right: (Long, Long))


def fraction(a: (Long, Long)): BigDecimal = {
  BigDecimal.exact(a._1).underlying().divide(BigDecimal.exact(a._2).underlying(), scale, RoundingMode
    .HALF_UP)
}

def mediant(a: (Long, Long), b: (Long, Long)): (Long, Long) = {
  (a._1 + b._1, a._2 + b._2)
}

def approximate(target: BigDecimal): Stream[(Long, Long)] = {
  lazy val approximateSub: Stream[ResultState] = {
    ResultState((0, 1), (1, 1), (1, 0)) #:: approximateSub.map { previous =>
      // left in Stern-Brocot Tree
      if (target < fraction(previous.approximation)) {
        val newRight = previous.approximation
        val newApproximation = mediant(previous.left, newRight)
        ResultState(previous.left, newApproximation, newRight)
      }
      // right in Stern-Brocot Tree
      else if (target > fraction(previous.approximation)) {
        val newLeft = previous.approximation
        val newApproximation = mediant(newLeft, previous.right)
        ResultState(newLeft, newApproximation, previous.right)
      } else {
        // number perfectly approximated (is rational, or precision exhausted)
        previous
      }
    }
  }
  approximateSub.map(_.approximation)
}

def error(approximation: (Long, Long)): BigDecimal = {
  (fraction(approximation) - PI).abs
}

def pickBetter(a: (Long, Long), b: (Long, Long)): (Long, Long) = {
  if (error(a) >= error(b)) {
    b
  } else {
    a
  }
}

def candidateStream(minDenominator: Long, maxDenominator: Long): Stream[(Long, Long)] = {
  val approximationCandidates = approximate(PI).span(_._2 < minDenominator)
  val insideRangeByDefault = approximationCandidates._2.takeWhile(_._2 <= maxDenominator)
  val multiplesOfBelowLowerBoundCandidates = approximationCandidates._1.map { pair =>
    if (minDenominator % pair._2 == 0) {
      (minDenominator / pair._2 * pair._1, minDenominator)
    } else {
      val multiplier = minDenominator / pair._2 + 1
      (pair._1 * multiplier, pair._2 * multiplier)
    }
  }.filter(_._2 <= maxDenominator)
  multiplesOfBelowLowerBoundCandidates #::: insideRangeByDefault
}

def approximatePI(minDenominator: Long, maxDenominator: Long): (Long, Long) = {
  candidateStream(minDenominator, maxDenominator)
    .map(approximation => {
      (approximation, error(approximation))
    })
    .minBy(_._2)._1

}

def checkDenominator(denominator: Long): ((Long, Long), BigDecimal) = {
  val lowerBound = (PI * BigDecimal(denominator)).toLong
  val upperBound = lowerBound + 1
  val lowerBoundDistance = (PI - fraction(lowerBound, denominator)).abs
  val upperBoundDistance = (fraction(upperBound, denominator) - PI).abs
  if (upperBoundDistance < lowerBoundDistance) {
    ((upperBound, denominator), upperBoundDistance)
  } else {
    ((lowerBound, denominator), lowerBoundDistance)
  }
}

def bruteForceApproximation(minDenominator: Long, maxDenominator: Long): (Long, Long) = {
  var result = ((0L, 0L), BigDecimal(Double.MaxValue))
  for (i <- minDenominator to maxDenominator) {
    val currentCheck = checkDenominator(i)
    if (currentCheck._2 < result._2) {
      result = currentCheck
    }
  }
  result._1
}

//ResultState((3, 1), (7, 2), (4, 1))

def isInRange(minDenominator: Long, maxDenominator: Long, x: (Long, Long)): Boolean = {
  minDenominator <= x._1 && x._2 <= maxDenominator
}

def combinedApproach(minDenominator: Long, maxDenominator: Long): (Long, Long) = {
  val bruteForceCutoff = 1000000
  val sternBrocot = approximatePI(minDenominator, maxDenominator)
  val searchSpace = (
    Math.max(minDenominator, sternBrocot._2 - bruteForceCutoff),
    Math.min(maxDenominator, sternBrocot._2 + bruteForceCutoff)
  )
  pickBetter(sternBrocot, bruteForceApproximation(searchSpace._1, searchSpace._2))
}

//
//val i = 460000
//
//val j = 480000
//
////for (i <- 1 to 100; j <- i to 1000) {
//val hackerEarth.a = bruteForceApproximation(i, j)
////val b = approximatePI(i, j)
//val b = combinedApproach(i, j)
//
//val good = hackerEarth.a == b
//val goodError = error(hackerEarth.a) >= error(b)
//if (!good) {
//  println((i, j) + "  " + (hackerEarth.a, b))
//  println((error(hackerEarth.a), error(b)))
//}

//val result2 = closestFractionToPi(1, 100000)

//val result = approximatePI(1, 842468587426513207L)
//println(result._1 + "/" + result._2)


//val target = fraction(1459097, 464445)
//val target = fraction(1458387, 464219)
//val aa = approximate(target).sliding(2).takeWhile({ pair =>
//  val hackerEarth.a = pair.toVector
//  hackerEarth.a(0) != hackerEarth.a(1)
//}).flatten.toVector.distinct.map(hackerEarth.a => (hackerEarth.a, error(hackerEarth.a)))
//
//val bb = approximate(PI).take(1000)
//
//aa.zip(bb).takeWhile(hackerEarth.a => hackerEarth.a._1._1._2 == hackerEarth.a._2._2) foreach println
//
//aa foreach println
//
//bb foreach println
//(candidateStream(i, j) take 200).sortBy(_._2) foreach println
//approximate(PI).span(_._2 < 332)._2.take(100) foreach println

