import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

val candidateIterator = new Iterator[Long] {
  private[this] val state = mutable.ArrayBuffer(2L, 3L, 5L, 7L)
  private[this] var i = 0


  def hasNext = true

  def next(): Long = {
    state ++= (state(i) * 10 + 2 :: state(i) * 10 + 3 :: state(i) * 10 + 5 :: state(i) * 10 + 7 :: Nil)
    i += 1
    state(i - 1)
  }
}

def isDigitPrime(digit: Char): Boolean = {
  digit == '2' || digit == '3' || digit == '5' || digit == '7'
}

def isMegaPrime(x: Long): Boolean = {
  x.toString.forall(digit => isDigitPrime(digit))
}


def sievePrimes(lowerBound: Long, upperBound: Long): Vector[Long] = {
  val sieve = mutable.HashSet.empty[Long]
  val limit = Math.sqrt(upperBound).toLong
  var nn = 0L
  var knn = 0L

  for (x <- 1L to limit; y <- 1L to limit) {
    val xx = x * x
    val yy = y * y
    var candidate = 4 * xx + yy
    if (candidate >= lowerBound &&
      candidate <= upperBound &&
      isMegaPrime(candidate) &&
      (candidate % 12 == 1 || candidate % 12 == 5)) {
      if (sieve.contains(candidate)) {
        sieve -= candidate
      } else {
        sieve += candidate
      }
    }

    candidate -= xx
    if (candidate >= lowerBound &&
      candidate <= upperBound &&
      isMegaPrime(candidate) &&
      candidate % 12 == 7) {
      if (sieve.contains(candidate)) {
        sieve -= candidate
      } else {
        sieve += candidate
      }
    }

    candidate = 3 * xx - yy
    if (candidate >= lowerBound &&
      candidate <= upperBound &&
      isMegaPrime(candidate) &&
      x > y &&
      candidate % 12 == 11) {
      if (sieve.contains(candidate)) {
        sieve -= candidate
      } else {
        sieve += candidate
      }
    }
  }

  for (n <- 5L to limit if sieve.contains(n)) {
    nn = n * n
    knn = nn
    breakable {
      for (_ <- 1L to upperBound) {
        if (knn > upperBound) break
        sieve -= knn
        knn += nn
      }
    }
  }

  if (upperBound >= 2 && lowerBound <= 2) sieve += 2
  if (upperBound >= 3 && lowerBound <= 3) sieve += 3
  sieve.toVector
}


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

def sievePrimses(lb: Int, ub: Int): ArrayBuffer[Int] = {
  val sieve = mutable.IndexedSeq.fill[Boolean](ub + 1)(false)
  val primes = ArrayBuffer[Int]()
  val limit = Math.sqrt(ub).toInt
  var n = 1
  var xx = 0
  var yy = 0
  var nn = 0
  var knn = 0

  for (x <- 1 to limit; y <- 1 to limit) {
    xx = x * x; yy = y * y
    n = 4 * xx + yy
    if (n <= ub && (n % 12 == 1 || n % 12 == 5)) sieve(n) = !sieve(n)
    n -= xx
    if (n <= ub && n % 12 == 7) sieve(n) = !sieve(n)
    n = 3 * xx - yy
    if (x > y && n <= ub && n % 12 == 11) sieve(n) = !sieve(n)
  }

  for (n <- 5 to limit if sieve(n)) {
    nn = n * n
    knn = nn
    breakable {
      for (k <- 1 to ub) {
        if (knn > ub) break
        sieve(knn) = false
        knn += nn
      }
    }
  }
  for (n <- 2 to 3) sieve(n) = true
  for (n <- lb to ub if sieve(n)) primes += n
  primes
}

println(sievePrimes(1,100).size)