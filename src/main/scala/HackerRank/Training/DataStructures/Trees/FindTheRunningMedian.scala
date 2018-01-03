package HackerRank.Training.DataStructures.Trees

import java.io.{ByteArrayInputStream, IOException, InputStream, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 1/3/2018
  */
private[this] object FindTheRunningMedian {

  import Reader._
  import Writer._

  private[this] val TEST_INPUT: Option[String] = None

  //------------------------------------------------------------------------------------------//
  // Solution
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val n = next[Int]()
    val values = next[Int, Vector](n)
    runningMedian(values).foreach(println)
  }

  def runningMedian[T](xs: Seq[T])(implicit numeric: Numeric[T]): Iterator[Double] = {
    import numeric._

    val it = xs.toIterator

    var i = 0

    new Iterator[Double] {
      private val maxHeap = mutable.PriorityQueue[T]()
      private val minHeap = mutable.PriorityQueue[T]()(Ordering.apply[T].reverse)
      private var lastOdd: Option[T] = None

      override def hasNext: Boolean = i < xs.size

      override def next(): Double = {
        val current = it.next()
        i += 1

        @inline def enqueueBothLeft(a: T, b: T): Unit = {
          //keep balance, both heaps must ve same size
          minHeap.enqueue(maxHeap.dequeue())
          maxHeap.enqueue(a)
          maxHeap.enqueue(b)
        }

        @inline def enqueueBothRight(a: T, b: T): Unit = {
          //keep balance, both heaps must ve same size
          maxHeap.enqueue(minHeap.dequeue())
          minHeap.enqueue(a)
          minHeap.enqueue(b)
        }

        @inline def bothInLeft(a: T, b: T): Boolean = {
          a <= maxHeap.head && b <= maxHeap.head
        }

        @inline def bothInRight(a: T, b: T): Boolean = {
          a >= minHeap.head && b >= minHeap.head
        }

        @inline def calculateMedianEvenLength(): Double = {
          (maxHeap.head + minHeap.head).toDouble() / 2.0
        }

        @inline def addFirstAndSecondToHeaps(): Unit = {
          val bigger = if (current < lastOdd.get) lastOdd.get else current
          val smaller = if (current > lastOdd.get) lastOdd.get else current

          minHeap.enqueue(bigger)
          maxHeap.enqueue(smaller)

          lastOdd = None
        }

        @inline def addCurrentAndLastToHeaps(): Unit = {
          val bigger = if (current < lastOdd.get) lastOdd.get else current
          val smaller = if (current > lastOdd.get) lastOdd.get else current

          if (bothInLeft(smaller, bigger)) {
            enqueueBothLeft(smaller, bigger)
          } else if (bothInRight(smaller, bigger)) {
            enqueueBothRight(smaller, bigger)
          } else {
            maxHeap.enqueue(smaller)
            minHeap.enqueue(bigger)
          }

          lastOdd = None
        }

        @inline def rememberCurrent(): Double = {
          lastOdd = Option(current)

          calculateMedianOddLength()
        }

        @inline def calculateMedianOddLength(): Double = {
          val isMedianSandwiched = current <= minHeap.head && current >= maxHeap.head
          val isMedianLeft = current <= maxHeap.head
          if (isMedianSandwiched) {
            current.toDouble()
          } else if (isMedianLeft) {
            maxHeap.head.toDouble
          } else {
            //          isMedianRight
            minHeap.head.toDouble
          }
        }

        if (i == 1) {
          lastOdd = Option(current)
          val median = current.toDouble()
          median
        } else if (i == 2) {
          addFirstAndSecondToHeaps()
          calculateMedianEvenLength()
        } else if (lastOdd.isDefined) {
          addCurrentAndLastToHeaps()
          calculateMedianEvenLength()
        } else {
          rememberCurrent()
          calculateMedianOddLength()
        }
      }
    }
  }

  //------------------------------------------------------------------------------------------//
  // Run
  //------------------------------------------------------------------------------------------//
  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val s = System.currentTimeMillis
    solve()
    flush()
    if (TEST_INPUT.isDefined) System.out.println(System.currentTimeMillis - s + "ms")
  }

  //------------------------------------------------------------------------------------------//
  // Input
  //------------------------------------------------------------------------------------------//
  private[this] final object Reader {

    private[this] implicit val in: InputStream = TEST_INPUT.fold(System.in)(s => new ByteArrayInputStream(s.getBytes))

    def next[T: ClassTag](): T = {
      implicitly[ClassTag[T]].runtimeClass match {
        case java.lang.Integer.TYPE => nextInt().asInstanceOf[T]
        case java.lang.Long.TYPE => nextLong().asInstanceOf[T]
        case java.lang.Double.TYPE => nextDouble().asInstanceOf[T]
        case java.lang.Character.TYPE => nextChar().asInstanceOf[T]
        case s if Class.forName("java.lang.String") == s => nextString().asInstanceOf[T]
        case b if Class.forName("scala.math.BigInt") == b => BigInt(nextString()).asInstanceOf[T]
        case b if Class.forName("scala.math.BigDecimal") == b => BigDecimal(nextString()).asInstanceOf[T]
        case _ => throw new RuntimeException("Unsupported input type.")
      }
    }

    def next[T, Coll[_]](reader: => T, n: Int)
                        (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += reader
      }
      builder.result()
    }

    def nextWithIndex[T, Coll[_]](reader: => T, n: Int)
                                 (implicit cbf: CanBuildFrom[Coll[(T, Int)], (T, Int), Coll[(T, Int)]]): Coll[(T, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((reader, i))
      }
      builder.result()
    }

    def next[T: ClassTag, Coll[_]](n: Int)
                                  (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (_ <- 0 until n) {
        builder += next[T]()
      }
      builder.result()
    }

    def nextWithIndex[T: ClassTag, Coll[_]](n: Int)
                                           (implicit cbf: CanBuildFrom[Coll[(T, Int)], (T, Int), Coll[(T, Int)]]): Coll[(T, Int)] = {
      val builder = cbf()
      builder.sizeHint(n)
      for (i <- 0 until n) {
        builder += ((next[T](), i))
      }
      builder.result()
    }

    def nextMultiLine[T: ClassTag](n: Int, m: Int): Seq[Seq[T]] = {
      val map = Vector.newBuilder[Vector[T]]
      var i = 0
      while (i < n) {
        map += next[T, Vector](m)
        i += 1
      }
      map.result()
    }

    private[this] def nextDouble(): Double = nextString().toDouble

    private[this] def nextChar(): Char = skip.toChar

    private[this] def nextString(): String = {
      var b = skip
      val sb = new java.lang.StringBuilder
      while (!isSpaceChar(b)) {
        sb.appendCodePoint(b)
        b = readByte()
      }
      sb.toString
    }

    private[this] def nextInt(): Int = {
      var num = 0
      var b = 0
      var minus = false
      while ( {
        b = readByte()
        b != -1 && !((b >= '0' && b <= '9') || b == '-')
      }) {}
      if (b == '-') {
        minus = true
        b = readByte()
      }
      while (true) {
        if (b >= '0' && b <= '9') {
          num = num * 10 + (b - '0')
        } else {
          if (minus) return -num else return num
        }
        b = readByte()
      }
      throw new IOException("Read Int")
    }

    private[this] def nextLong(): Long = {
      var num = 0L
      var b = 0
      var minus = false
      while ( {
        b = readByte()
        b != -1 && !((b >= '0' && b <= '9') || b == '-')
      }) {}
      if (b == '-') {
        minus = true
        b = readByte()
      }
      while (true) {
        if (b >= '0' && b <= '9') {
          num = num * 10 + (b - '0')
        } else {
          if (minus) return -num else return num
        }
        b = readByte()
      }
      throw new IOException("Read Long")
    }

    private[this] val inputBuffer = new Array[Byte](1024)
    private[this] var lenBuffer = 0
    private[this] var ptrBuffer = 0

    private[this] def readByte()(implicit in: java.io.InputStream): Int = {
      if (lenBuffer == -1) throw new InputMismatchException
      if (ptrBuffer >= lenBuffer) {
        ptrBuffer = 0
        try {
          lenBuffer = in.read(inputBuffer)
        } catch {
          case _: IOException =>
            throw new InputMismatchException
        }
        if (lenBuffer <= 0) return -1
      }
      inputBuffer({
        ptrBuffer += 1
        ptrBuffer - 1
      })
    }

    private[this] def isSpaceChar(c: Int) = !(c >= 33 && c <= 126)

    private[this] def skip = {
      var b = 0
      while ( {
        b = readByte()
        b != -1 && isSpaceChar(b)
      }) {}
      b
    }
  }

  //------------------------------------------------------------------------------------------//
  // Output
  //------------------------------------------------------------------------------------------//
  private[this] final object Writer {

    private[this] val out = new PrintWriter(System.out)

    def flush(): Unit = out.flush()

    def println(x: Any): Unit = out.println(x)

    def print(x: Any): Unit = out.print(x)
  }

}