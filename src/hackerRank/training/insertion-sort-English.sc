
import java.net.URL
import java.util.Scanner

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


//  val sc = new java.util.Scanner(System.in)
val sc = new Scanner(new URL("https://hr-testcases-us-east-1.s3.amazonaws.com/49/input13.txt?AWSAccessKeyId=AKIAJAMR4KJHHUS76CYQ&Expires=1490286699&Signature=NpgJXMLJZ3RVqOEtSAjr6QQfo%2F8%3D&response-content-type=text%2Fplain").openStream)
val builder = List.newBuilder[Int]
val t = sc.nextInt
for (testcase <- 1 to t) {
  val n = sc.nextInt
  for (arr_i <- 0 until n) {
    builder += sc.nextInt
  }
  println(countInversions(builder.result))
  builder.clear
}
sc.close()
}
