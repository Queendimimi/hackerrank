import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def sum(fibonacciNumbers: ListBuffer[BigInt]): BigInt = {
  fibonacciNumbers.sum
}

def evenFibonacci(upperBound: Long): ListBuffer[BigInt] = {
  val buffer = ListBuffer.empty[BigInt]
  buffer += 2
  evenFibonacciRecursion(2, 0)

  @tailrec
  def evenFibonacciRecursion(a: BigInt, b: BigInt): Unit = {
    val next = 4 * a + b
    if (next <= upperBound) {
      buffer += next
      evenFibonacciRecursion(next, a)
    }
  }

  buffer
}