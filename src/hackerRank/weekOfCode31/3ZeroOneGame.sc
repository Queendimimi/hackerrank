import scala.util.Random

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

def main2(args: Array[String]) {

  val sc = new java.util.Scanner(System.in)
  val g = sc.nextInt()
  var a0 = 0
  while (a0 < g) {
    var count = 0
    var last = -1
    var lastLast = -1
    val n = sc.nextInt()
    for (_ <- 0 until n) {
      val current = sc.nextInt()
      if (current != 1 && last != 1) {
        count += 1
      }
      if (current == 1 && last == 1 && lastLast != 1) {
        count -= 1
      }
      lastLast = last
      last = current
    }
    // If Alice wins, print 'Alice' on a new line; otherwise, print 'Bob'
    println(count % 2)
    a0 += 1
  }
}

def test(in: Vector[Int]) = {
  val input = in.dropWhile(_ == 1)
  var count = 0
  var last = -1
  var lastLast = -1
  for (current <- input) {
    if (current != 1 || last != 1) {
//      println("a")
      count += 1
    }
    if (current == 1 && last == 1 && lastLast != 1) {
//      println("b")
      count -= 1
    }
    lastLast = last
    last = current
  }
  println(count)
  if (count % 2 == 0) "Bob" else "Alice"

}
// If Alice wins, print 'Alice' on a new line; otherwise, print 'Bob'

val tests = List(
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

test(Vector(1,0,0,1,0))


//tests.map(_ + "110001111000").map(solve) foreach println

//tests.map(solve) foreach println

tests.map(_.toCharArray.map(Character.getNumericValue).toVector).map(test) foreach println
