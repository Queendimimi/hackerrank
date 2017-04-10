def countInversions(input: List[Int]): Long = {
  sortAndCountInversion(input)._2
}

def sortAndCountInversion(list: List[Int], count: Long = 0): (List[Int], Long) = {
  if (list.length <= 1) {
    (list, 0)
  } else {
    val (rightList, leftList) = list.splitAt(list.length / 2)
    val (sortedRightList, rightCount) = sortAndCountInversion(rightList, count)
    val (sortedLeftList, leftCount) = sortAndCountInversion(leftList, count)
    val (sortedMergedList, mergedCount) = merge(sortedRightList, sortedLeftList)
    (sortedMergedList, rightCount + leftCount + mergedCount)
  }
}

def merge(left: List[Int], right: List[Int], count: Long = 0): (List[Int], Long) = {
  (left, right) match {
    case (Nil, _) => (right, count)
    case (_, Nil) => (left, count)
    case (leftHead :: leftTail, rightHead :: rightTail) =>
      if (leftHead > rightHead) {
        val (list, rightCount) = merge(left, rightTail, count)
        (rightHead :: list, count + left.length + rightCount)
      } else {
        val (list, leftCount) = merge(leftTail, right, count)
        (leftHead :: list, count + leftCount)
      }
  }
}

import scala.collection.mutable.ListBuffer
import scala.io.Source

val test = Source.fromURL("https://d3c33hcgiwev3.cloudfront.net/_bcb5c6658381416d19b01bfc1d3993b5_IntegerArray.txt?Expires=1488499200&Signature=BQjomBOlsPk-ZkPsCe9CQdjkhOZqX0Z6GXarVtTg7uCk0PkaX9sBimSsxXF9SHBbrGWndaaNnB794~cYSH03p9RYZrB7pErY9nFM7FCqM5mYZTeP8wJ7TIWbVIkSls~yLmLbfuG-pat-HQ5jwQCoh1-WUup1JEDbtx3Z5-W7CLs_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A")
val builder = new ListBuffer[Int]()
for (line <- test.getLines) {
  builder += line.toInt
}

println(countInversions(builder.result()))



