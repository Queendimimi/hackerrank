import scala.collection.mutable

object PalindromeCandidates extends Iterable[Int] {
  private[this] val queue = generateGenerators()

  def iterator = new Iterator[Int] {
    def hasNext = true

    def next:Int = {
      val next = queue.dequeue()
      val number = next.next
      next.calculateNext()
      queue.enqueue(next)
      number
    }
  }
}

def findBiggestPalindrome(upperBound: Int) = {
  val candidateIterator = PalindromeCandidates.iterator
  candidateIterator.find(n => isPalindrome(n) && n < upperBound)
}

def isPalindrome(n: Int): Boolean = {
  val pair = n.toString.splitAt(3)
  pair._1 == pair._2.reverse
}

def generateGenerators(): mutable.PriorityQueue[Generator] = {
  val queue = mutable.PriorityQueue[Generator]()(GeneratorOrdering)
  for {
    i <- 999 to 100 by -1
  } yield {
    queue += Generator(i)
  }
  queue
}

case class Generator(n: Int) {
  var next: Int = n * n

  def calculateNext(): Unit = {
    val potentialNext = next - n
    if (potentialNext >= 101101) {
      next = potentialNext
    }
  }
}

object GeneratorOrdering extends Ordering[Generator] {
  def compare(a: Generator, b: Generator): Int = a.next compare b.next
}