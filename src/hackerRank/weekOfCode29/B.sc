def sort(input: List[String]): List[String] = {
  val (left, right) = input.splitAt(input.length / 2)
  if (input.length <= 1) {
    input
  } else {
    merge(sort(left), sort(right))
  }
}

def merge(left: List[String], right: List[String]): List[String] = {
  (left, right) match {
    case (Nil, _) => right
    case (_, Nil) => left
    case (lHead :: lTail, rHead :: rTail) =>
      if (isBigger(rHead, lHead)) {
        lHead :: merge(lTail, right)
      } else {
        rHead :: merge(left, rTail)
      }
  }
}

def isBigger(a: String, b: String): Boolean = {
  if (a.length != b.length) {
    a.length > b.length
  } else {
    val firstTwoDifferentDigits = a.toStream.zip(b.toStream)
      .map(x => (x._1.asDigit, x._2.asDigit))
      .find(x => x._1 != x._2)
      .getOrElse((0, 1))
    if(firstTwoDifferentDigits._1 > firstTwoDifferentDigits._2) {
      true
    } else {
      false
    }
  }
}
println(sort(List("6",
  "31415926535897932384626433832795",
  "1",
  "3",
  "10",
  "3",
  "5").toList).mkString("\n"))

