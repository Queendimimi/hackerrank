import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer
import scala.io.Source



def quickSort[T, Col](xs: Col)
                     (implicit c2s: Col <:< SeqLike[T, Col],
                      cbf: CanBuildFrom[Col, T, Col],
                      ordering: Ordering[T]): Col = {
  import ordering._
  if (xs.length <= 1) {
    xs
  } else {
    def pivot(list: Col): (Col, T, Col) = {
      val (left, middle, right) = list.toBuffer.tail.foldLeft((cbf(), list.head, cbf())
      ) { (result, item) =>
        val (left, pivot, right) = result
        if (item < pivot) (left += item, pivot, right) else (left, pivot, right += item)
      }
      (left.result(), middle, right.result())
    }

    def pivotAt(list: Col, index: Int): (Col, T, Col) = {
      def swap(): Col = {
        val buffer = list.toBuffer
        val builder = cbf()
        val first = list.head
        buffer(0) = buffer(index)
        buffer(index) = first
        builder ++= buffer
        builder.result()
      }

      pivot(swap())
    }

    def median(list: Col): Int = {
      val head = (list.head, 1)
      val last = (list.last, list.length - 1)
      val middle =
        if (list.length % 2 == 0) {
          (list(list.length / 2 - 1), list.length / 2 - 1)
        } else {
          (list(list.length / 2), list.length / 2)
        }
      val sorted = Vector(head, middle, last).sortBy(_._1)
      sorted(2)._2
    }

    val (left, middle, right) = pivotAt(xs, median(xs))
    val builder = cbf()
    builder.sizeHint(xs.size)
    builder ++= quickSort(left) += middle ++= quickSort(right)
    builder.result()
  }
}

val test = Source.fromURL("https://d3c33hcgiwev3.cloudfront.net/_32387ba40b36359a38625cbb397eee65_QuickSort.txt?Expires=1488585600&Signature=QUwHZ7jjZqFjg9VQ5ElxSgBQiy8OnRB~P0m4xRbAYHAZVg-DGv4rkhCqj-QXCkxTJ0IzlKAmRk3iUncjvq57Eo2EAnubvIW0Y4mEtdGZL97NhUQmjk2PNkm0nKdFPCbbO1krFLohlag0Eqy5sXjxVyTsaalDRV5xn~7LVmtIAgg_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A")
val builder = new ListBuffer[Int]()
for (line <- test.getLines) {
  builder += line.toInt
}


var a = Vector(9, 5, 4, 13, 18, 423, 123)
println(quickSort(builder.result()))
