def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val word = sc.next()
  // Print 'Yes' if the word is beautiful or 'No' if it is not.
  val vowels = Set('a', 'e', 'i', 'o', 'u', 'y')
  if (word.length > 1) {
    val isBeautiful = word.sliding(2, 1).forall { pair =>
      val a = pair(0)
      val b = pair(1)

      a != b && (!vowels.contains(a) || !vowels.contains(b))
    }
    if (isBeautiful) println("Yes") else println("No")
  } else {
    println("Yes")
  }
}



