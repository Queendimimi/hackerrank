import scala.collection.mutable.ArrayBuffer

def ()

case class Matrix(rows: Int, columns: Int) {
  val matrix = ArrayBuffer.fill(rows, columns)(0)
}