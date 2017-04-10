def mean(input: Array[Int]): Double = {
  input.sum / input.length.toDouble
}

def median(input: Array[Int]): Double = {
  val sorted = input.sorted
  if (input.length % 2 == 0) {
    (sorted(input.length / 2) + sorted(input.length / 2 - 1)) / 2.0
  } else {
    sorted(input.length / 2)
  }
}

def mode(input: Array[Int]): Int = {
  val count = input.groupBy(a => a).values.map(array => (array.length, array.head)).toVector

  val maxCount = count.filter(pair => pair._1 == count.maxBy(_._1)._1)

  maxCount.minBy(_._2)._2
}

def round(input: Double): Double = {
  Math.round(input * 10.0) / 10.0
}

val c = Vector(64630, 11735, 14216, 99233, 14470, 4978, 73429, 38120, 51135, 67060).toArray
println(round(mean(c)))
println(round(median(c)))
println(mode(c))