import java.math.{RoundingMode, BigDecimal => BigDec}

import scala.collection.mutable

val scale = 50

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

def error(value: Double): Double = {
  Math.abs(Math.rint(value) - value)
}

def isValid(value: Double): Boolean = {
  error(value) <= 0.000000000001
}

case class Point(x: Int, y: Int)

case class Plane() {
  private[this] val plane = mutable.Set.empty[Point]

  def addPoint(point: Point): Unit = {
    plane += point
  }

  def distance(point: Point): Double = {
    sqrt(point.x * point.x + point.y * point.y).toDouble
  }

  def sumDistance: Double = {
    plane.map(point => distance(point)).sum
  }
}

for (x <- 0 to 12; y <- 1 to 12;
     a <- x to 12; b <- y to 12;
     c <- a to 12; d <- b to 12;
     e <- c to 12; f <- d to 12) {
  //     hackerEarth.a <- 1 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12;
  //     hackerEarth.a <- 0 to 12; b <- 0 to 12

  val plane = Plane()
  plane.addPoint(Point(a, b))
  plane.addPoint(Point(x, y))
  plane.addPoint(Point(e, f))
  plane.addPoint(Point(c, d))
  if (isValid(plane.sumDistance)) {
    println((x, y), (a, b), (c, d), (e, f))
  }
}