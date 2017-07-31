import scala.annotation.tailrec
import scala.collection.mutable

// O(n + log(n))
def secondHighest(input: Vector[Integer]): Integer = {
  var secondHighest = Integer.MIN_VALUE

  var adjustedInput = input.toBuffer

  if (input.size % 2 != 0) {
    adjustedInput -= input.last
  }

  @tailrec
  def highest(input: mutable.Buffer[Integer]): Integer = {
    if (input.length == 1) {
      input.head
    } else {
      highest(input.grouped(2).map({ pair =>
        if (pair.head > pair(1)) {
          if (secondHighest < pair(1)) {
            secondHighest = pair(1)
          }
          pair.head
        } else if (pair.head < pair(1)) {
          if (secondHighest < pair.head) {
            secondHighest = pair.head
          }
          pair(1)
        } else {
          pair(1)
        }
      }).toBuffer)
    }
  }

  var highestValue = highest(adjustedInput)
  if (highestValue < input.last) {
    secondHighest = highestValue
    highestValue = input.last
  }
  if (secondHighest < input.last && input.last < highestValue) input.last else secondHighest
}

println(secondHighest(Vector(10, 9, 5, 4, 11, 100, 120, 130, 123)))

