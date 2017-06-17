import scala.collection.immutable.VectorBuilder

val consonants = Vector(
  'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm',
  'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z')
val vowels = Vector('a', 'e', 'i', 'o', 'u')
def generateMelodiousPasswords(n: Int): Unit = {
  def vowelPath = {
    var result = Vector("")
    for (i <- 1 to n) {
      if (i % 2 == 0) {
        result = appendConsonants(result, i)
      } else {
        result = appendVowels(result, i)
      }
    }
    result
  }

  def consonantPath = {
    var result = Vector("")
    for (i <- 1 to n) {
      if (i % 2 == 0) {
        result = appendVowels(result, i)
      } else {
        result = appendConsonants(result, i)
      }
    }
    result
  }

  def appendConsonants(current: Vector[String], k: Int) = append(current, consonants, k)

  def appendVowels(current: Vector[String], k: Int) = append(current, vowels, k)

  def append(current: Vector[String], newChars: Vector[Char], k: Int): Vector[String] = {
    val builder = new VectorBuilder[String]
    //noinspection ScalaUselessExpression
    builder.sizeHint(current.length * newChars.length)

    for (i <- current.indices; j <- newChars.indices) {
      val res = current(i) + newChars(j)
      builder += res
      if(n == k) {
        println(res)
      }
    }
    builder.result
  }

  consonantPath
  vowelPath
}



generateMelodiousPasswords(1)

