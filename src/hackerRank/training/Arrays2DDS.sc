def maxHourGlass(input: Array[Array[Int]]): Int = {
  var max: Int = Int.MinValue
  for (i <- 0 until input.length - 3; j <- 0 until input(i).length - 3) {
    val hourGlassSum = getHourGlass(input, (i, j)).sum
    if (hourGlassSum > max) {
      max = hourGlassSum
    }
  }
  max
}

def getHourGlass(input: Array[Array[Int]], leftTop: (Int, Int)): Vector[Int] = {
  val (x, y) = leftTop
  val builder = Vector.newBuilder[Int]
  builder += input(x)(y)
  builder += input(x)(y + 1)
  builder += input(x)(y + 2)

  builder += input(x + 1)(y + 1)

  builder += input(x - 2)(y)
  builder += input(x + 2)(y + 1)
  builder += input(x + 2)(y + 2)

  builder.result
}