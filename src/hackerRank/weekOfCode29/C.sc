case class Pixel(x: Int, y: Int, var value: Boolean = false) {
  override def toString: String = {
    if (value) {
      "#"
    } else {
      "."
    }
  }
}

case class Square(a: (Double, Double), b: (Double, Double), c: (Double, Double), d: (Double, Double)) {

  def isInside(point: (Double, Double)): Boolean = {
    val AB = EuclideanVector(a, b)
    val AP = EuclideanVector(a, point)
    val BC = EuclideanVector(b, c)
    val BP = EuclideanVector(b, point)
    val ABdotAP = scalarProduct(AB, AP)
    val ABdotAB = scalarProduct(AB, AB)
    val BCdotBP = scalarProduct(BC, BP)
    val BCdotBC = scalarProduct(BC, BC)
    0 <= ABdotAP && ABdotAP <= ABdotAB && 0 <= BCdotBP && BCdotBP <= BCdotBC
  }
}

def scalarProduct(a: EuclideanVector, b: EuclideanVector): Double = {
  a.scalarProduct(b)
}

case class EuclideanVector(a: (Double, Double), b: (Double, Double)) {
  val x: Double = b._1 - a._1
  val y: Double = b._2 - a._2

  def scalarProduct(b: EuclideanVector): Double = {
    x * b.x + y * b.y
  }
}

case class Canvas(width: Int, height: Int) {
  private[this] val canvas = Array.tabulate[Pixel](width, height) {
    (i, j) => Pixel(i, j)
  }

  def setPixel(x: Int, y: Int): Unit = {
    canvas(x)(y).value = true
  }

  def drawCircle(center: (Int, Int), radius: Int): Unit = {
    for {
      x <- 0 until width
      y <- 0 until height
      if distanceSquared((x, y), center) <= radius * radius
    } yield setPixel(x, y)
  }

  def drawSquare(A: (Int, Int), C: (Int, Int)): Unit = {
    val B = ((A._1 + C._1 + C._2 - A._2) / 2.0, (A._2 + C._2 + A._1 - C._1) / 2.0)
    val D = ((A._1 + C._1 + A._2 - C._2) / 2.0, (A._2 + C._2 + C._1 - A._1) / 2.0)
    val square = Square((A._1.toDouble, A._2.toDouble), B, (C._1.toDouble, C._2.toDouble), D)
    for {
      x <- 0 until width
      y <- 0 until height
      if square.isInside((x.toDouble, y.toDouble))
    } yield setPixel(x, y)
  }

  def distanceSquared(a: (Int, Int), b: (Int, Int)): Int = {
    (a._1 - b._1) * (a._1 - b._1) + (a._2 - b._2) * (a._2 - b._2)
  }


  override def toString: String = {
    val result = StringBuilder.newBuilder
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        result.append(canvas(x)(y))
      }
      result.append("\n")
    }
    result.toString
  }
}


val a = Canvas(20, 16)
a.drawCircle((200, 60), 200)
a.drawSquare((16, 14), (8, 14))
println(a)

