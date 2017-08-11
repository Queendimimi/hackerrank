package HackerRank.Training.DataStructures.Stacks

import java.io.{ByteArrayInputStream, IOException, InputStream, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 8/6/2017
  */
private[this] object PoisonousPlants {

  import Reader._
  import Writer._

  private[this] val TEST_INPUT: Option[String] = None

  //------------------------------------------------------------------------------------------//
  // Solution
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val n = next[Int]()
    println(
      solveInLinearTime(next[Long, Vector](n).map(-_))
    )
    //    println(
    //      next[Long, Vector](n)
    //        .splitDependingOnPrevious[List, List]((current, previous) => current > previous)
    //        .map(xs => new CustomStack[Long](xs))
    //        .solve()
    //    )
  }

  private[this] def solveInLinearTime[T: Ordering](xs: Seq[T]) = {
    import Ordering.Implicits._
    import scala.util.control.Breaks._

    var max = 0
    val stack = mutable.ArrayStack[(T, Int)]()
    for (currentElement <- xs) {
      if (stack.isEmpty) {
        stack.push((currentElement, 0))
      } else {
        if (currentElement < stack.head._1) {
          max = Math.max(max, 1)
          stack.push((currentElement, 1))
        } else {
          var head = stack.head
          var pr = head._2
          breakable {
            while (stack.nonEmpty && head._1 <= currentElement) {
              stack.pop()
              if (stack.isEmpty) {
                break
              }
              pr = Math.max(pr, head._2)
              head = stack.head
            }
          }
          if (stack.isEmpty) {
            stack.push((currentElement, 0))
          } else {
            stack.push((currentElement, pr + 1))
            max = Math.max(max, pr + 1)
          }
        }
      }
    }
    max
  }

  private[this] implicit final class SeqExtension[T](xs: Seq[T]) {
    /**
      *
      * @param predicate ???
      * @param cbfA      ???
      * @param cbfB      ???
      * @tparam CollA outer collection
      * @tparam CollB inner collection
      * @return
      */
    def splitDependingOnPrevious[CollA[_], CollB[_]]
    (predicate: (T, T) => Boolean)
    (implicit cbfA: CanBuildFrom[CollB[T], T, CollB[T]],
     cbfB: CanBuildFrom[CollA[CollB[T]], CollB[T], CollA[CollB[T]]]): CollA[CollB[T]] = {
      val outerBuilder = cbfB()
      //at start prev == head
      var previous = xs.headOption
      var innerBuilder = cbfA()
      for (i <- xs.indices) {
        if (previous.fold(false)(prev => predicate(xs(i), prev))) {
          outerBuilder += innerBuilder.result()
          innerBuilder = cbfA()
        }
        val current = xs(i)
        innerBuilder += current
        previous = Option(current)
        //if this is the last iteration add remaining elements to result
        if (i == xs.size - 1) {
          outerBuilder += innerBuilder.result()
        }
      }
      outerBuilder.result()
    }
  }

  private[this] implicit final class CustomList[T: Ordering](private var underlying: List[CustomStack[T]]) {
    def solve(): Int = {
      var i = 0
      while (underlying.size != 1) {
        i += 1
        popTail()
        merge()
      }
      i
    }

    private[this] def popTail(): Unit = {
      underlying.tail.foreach(_.pop())
    }

    private[this] def merge(): Unit = {
      import Ordering.Implicits._

      underlying = underlying
        .filter(_.nonEmpty)
        .foldLeft(new ArrayBuffer[CustomStack[T]]()) { case (acm, value) =>
          if (value.headOption.fold(false)(head =>
            acm.lastOption.fold(false)(nonEmptyAcm =>
              nonEmptyAcm.lastOption.fold(false)(_ >= head)))) {
            acm.last ++ value
            acm
          } else {
            acm += value
          }
        }.toList
    }
  }

  private[this] final class CustomStack[T](private var underlying: List[T] = List.empty[T]) {
    private var lastElement = underlying.lastOption

    def pop(): Option[T] = {
      val res = underlying.headOption
      res.foreach(_ => underlying = underlying.tail)
      if (underlying.isEmpty) {
        lastElement = None
      }
      res
    }

    def push(x: T): Unit = {
      if (underlying.isEmpty) {
        lastElement = Some(x)
      }
      underlying = x :: underlying
    }

    def lastOption: Option[T] = lastElement

    def headOption: Option[T] = underlying.headOption

    def ++(b: CustomStack[T]): Unit = {
      underlying = underlying ::: b.underlying
      lastElement = b.lastElement
    }

    def nonEmpty: Boolean = underlying.nonEmpty

    override def toString: String = underlying.toString

    override def hashCode(): Int = underlying.hashCode()

    override def equals(obj: scala.Any): Boolean = underlying.equals(obj)
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