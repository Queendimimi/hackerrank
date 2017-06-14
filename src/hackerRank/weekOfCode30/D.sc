import scala.collection.mutable

def minimalCost(poles: Vector[(Int, Int)], numberOfBuckets: Int): Int = {
  def findOptimalBucket(poles: Vector[(Int, Int)]): (Int, Int, Int) = {
    var (lowerHalfCost, upperHalfCost, bucketPosition) = (Int.MaxValue / 2, Int.MaxValue / 2, 0)
    for (i <- 1 to poles.length) {
      val (lower, upper) = poles.splitAt(i)
      var upperCost = -1
      var lowerCost = -1
      if (upper.isEmpty) {
        lowerCost = sumCosts(lower, lower.head._1)
        upperCost = 0
      } else {
        lowerCost = sumCosts(lower, lower.head._1)
        upperCost = sumCosts(upper, upper.head._1)
      }
      if (lowerCost + upperCost < lowerHalfCost + upperHalfCost) {
        lowerHalfCost = lowerCost
        upperHalfCost = upperCost
        bucketPosition = i
      }
    }
    (lowerHalfCost, upperHalfCost, bucketPosition)
  }

  if (numberOfBuckets == 1) {
    sumCosts(poles, poles.head._1)
  } else {
    val poleSplits = mutable.ListBuffer[(Vector[(Int, Int)], Int)]()
    val stacks = mutable.ListBuffer[Int](0)

    val (_, _, index) = findOptimalBucket(poles)
    stacks += index
    val (lower, upper) = poles.splitAt(index)
    poleSplits += ((lower, 0))
    poleSplits += ((upper, lower.size))

    for (_ <- 2 until numberOfBuckets) {
      val candidates = mutable.ListBuffer[(Vector[Int], Int)]()
      for ((list, shift) <- poleSplits) {
        val (_, _, index) = findOptimalBucket(list)
        candidates += ((stacks.result.toVector :+ (index + shift), index + shift))
      }
      if (candidates.nonEmpty) {
        val bestCandidate = candidates.minBy(p => calculateCost(poles, p._1))
        stacks += bestCandidate._2
        val tempStacks = stacks.result.toVector
        poleSplits.clear()
        poleSplits ++ sliceAll(poles, tempStacks)
        candidates.clear()
      }
    }
    calculateCost(poles, stacks.result.toVector)
  }
}



def merge(x: Int, y: Int, list: Seq[(Int, Int, Int)]): (Int, Int, Int) = {
  def merge2(x: Int,
             y: Int,
             a: (Int, Int, Int),
             b: (Int, Int, Int)): (Int, Int, Int) = {
    if (x + b._1 + b._2 < y + a._1 + a._2) {
      b
    } else {
      a
    }
  }

  list.reduce(merge2(x, y, _, _))
}



def sumCosts(poles: Vector[(Int, Int)], bucketAltitude: Int): Int = {
  var sum = 0
  for (pole <- poles) {
    sum += calculateCost(pole, bucketAltitude)
  }
  sum
}

def calculateCost(pole: (Int, Int), bucketAltitude: Int): Int = {
  val (altitude, weight) = pole
  weight * Math.abs(altitude - bucketAltitude)
}

def calculateCost(poles: Vector[(Int, Int)], bucketIndices: Vector[Int]): Int = {
  var sum = 0
  for (i <- 0 until bucketIndices.length - 1) {
    val lower = poles.slice(bucketIndices(i), bucketIndices(i + 1))
    if (lower.nonEmpty) {
      sum += sumCosts(lower, lower.head._1)
    }
  }
  val lower = poles.splitAt(bucketIndices.last)._2
  if (lower.nonEmpty) {
    sum += sumCosts(lower, lower.head._1)
  }
  sum
}

def sliceAll(poles: Vector[(Int, Int)], splits: Vector[Int]): mutable.ListBuffer[(Vector[(Int, Int)], Int)]
= {
  val builder = mutable.ListBuffer[(Vector[(Int, Int)], Int)]()
  for (i <- 0 until splits.length - 1) {
    builder += ((poles.slice(splits(i), splits(i + 1)), poles.splitAt(splits(i))._1.length))
  }
  builder += ((poles.slice(splits.last, poles.length), poles.splitAt(splits.last)._1.length))
  builder
}




val p = Vector((10, 15), (12, 17), (16, 18), (18, 13), (30, 10), (32, 1))

val p2 = Vector((10, 15), (12, 17), (18, 13), (30, 10), (32, 1))
val p3 = Vector((12, 17), (18, 13), (30, 10), (32, 1))

val p4 = Vector((20, 1), (30, 1), (40, 1))

println(minimalCost(p, 4))

println(minimalCost(p2, 1))

println(minimalCost(p3, 1))

println(minimalCost(p4, 1))

minimalCost(p, 5)
println(calculateCost(p, Vector(0, 2)))
//println(minimalCost(p4, 1))

sliceAll(p, Vector(0, 2))