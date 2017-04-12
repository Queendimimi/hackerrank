import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

def isWin(game: String) = {
  if (game.length % 2 == 0) 0 else 1
}

def solve(str: String) = {
  val games = str.split("11+").filter(_.nonEmpty)
  val result = games.view.map { game =>
    var result = game
    if (game.head == '1') result = result.drop(1)
    if (game.last == '1') result = result.dropRight(1)
    result
  }.filter(_.length > 2).map(isWin).fold(0)(_ ^ _)
  if (result == 0) "Bob" else "Alice"
}

def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val g = sc.nextInt()
  var a0 = 0
  while (a0 < g) {
    val n = sc.nextInt()
    val word = StringBuilder.newBuilder
    for (_ <- 0 until n) {
      word += Character.forDigit(sc.nextInt(), 10)
    }
    // If Alice wins, print 'Alice' on a new line; otherwise, print 'Bob'
    println(solve(word.result()))
    a0 += 1
  }
}


def solve2(input: mutable.Buffer[Int]): String = {
  //  val input = in.dropWhile(_ == 1).reverse.dropWhile(_ == 1).reverse
  if (input.length < 3) return "Bob"
  input.append(1, 1, 1)
  input.prepend(1, 1, 1)
  for (i <- 3 until input.length - 3) {
    if (input(i) == 1 && input(i - 1) == 0 && input(i + 1) == 0) {
      input(i) = 0
    }
    if (input(i) == 0 && input(i - 1) == 1 && input(i + 1) == 1 && input(i + 2) == 1
      && input(i - 2) == 1) {
      input(i) = 1
    }
    if (input(i) == 0 && input(i - 1) == 1 && input(i + 1) == 0 && input(i + 2) == 1
      && input(i - 2) == 1 && input(i + 3) == 1) {
      input(i) = 1
    }
    if (input(i) == 0 && input(i - 1) == 0 && input(i + 1) == 1 && input(i + 2) == 1
      && input(i - 2) == 1) {
      input(i) = 1
    }
  }
  if (input.count(_ == 0) % 2 == 0) "Bob" else "Alice"
}

def main2(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val g = sc.nextInt()
  var a0 = 0
  while (a0 < g) {
    val n = sc.nextInt()
    val builder = ArrayBuffer.empty[Int]
    for (_ <- 0 until n) {
      builder += sc.nextInt()
    }
    // If Alice wins, print 'Alice' on a new line; otherwise, print 'Bob'
    println(solve2(builder))
    a0 += 1
  }
}


def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val g = sc.nextInt()
  var a0 = 0
  var count = 0
  var i = -1

  while (a0 < g) {
    val n = sc.nextInt()
    val input = ArrayBuffer.empty[Int]
    if (n < 3) {
      println("Bob")
    } else {
      count = 0
      input.prepend(1, 1, 1)
      for (j <- 0 until n) {
        if (j == n - 1) input.append(1, 1, 1)

        i = j + 3
        if (input(i) == 1 && input(i - 1) == 0 && input(i + 1) == 0) {
          input(i) = 0
        }
        if (input(i) == 0 && input(i - 1) == 1 && input(i + 1) == 1 && input(i + 2) == 1
          && input(i - 2) == 1) {
          input(i) = 1
        }
        if (input(i) == 0 && input(i - 1) == 1 && input(i + 1) == 0 && input(i + 2) == 1
          && input(i - 2) == 1 && input(i + 3) == 1) {
          input(i) = 1
        }
        if (input(i) == 0 && input(i - 1) == 0 && input(i + 1) == 1 && input(i + 2) == 1
          && input(i - 2) == 1) {
          input(i) = 1
        }
        if (input(i) == 0) count += 1
      }

      // If Alice wins, print 'Alice' on a new line; otherwise, print 'Bob'
      if (count % 2 == 0) "Bob" else "Alice"
      a0 += 1
    }
  }
}

val tests = List(
  "1",
  "11",
  "10010",
  "10100",
  "01001",
  "10001",
  "01010",
  "01100",
  "00101",
  "00011",
  "00110",
  "11000")

solve2(ArrayBuffer(1, 0, 0, 1, 0))


//tests.map(_ + "110001111000").map(solve) foreach println

tests.map(_ + "110001111000").map(solve) foreach println

tests.map(_ + "110001111000").map(_.toCharArray.map(Character.getNumericValue).toBuffer).map(solve2) foreach println
