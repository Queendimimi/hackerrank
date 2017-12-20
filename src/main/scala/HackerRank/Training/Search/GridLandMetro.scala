package HackerRank.Training.Search

import java.io.{ByteArrayInputStream, IOException, InputStream, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{NumericRange, Vector}
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 8/7/2017
  */
private[this] object GridLandMetro {

  import Reader._
  import Writer._

  private[this] val TEST_INPUT: Option[String] = None

  //------------------------------------------------------------------------------------------//
  // Solution
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val n = next[Long]()
    val m = next[Long]()
    val k = next[Int]()
    val trackBuilder = mutable.Map.empty[Int, mutable.Builder[NumericRange[Int], List[NumericRange[Int]]]]

    next[Track[Int], Vector](
      new Track(next[Int](), NumericRange.inclusive(next[Int](), next[Int](), 1)),
      k)
      .foreach(track =>
        trackBuilder.getOrElseUpdate(track.row, List.newBuilder[NumericRange[Int]])
          += NumericRange.inclusive(track.start, track.end, 1)
      )

    val cellsWithTrack = trackBuilder.map { case (key, tracks) =>
      tracks.result()
        .collapse
        .foldLeft(0L) { case (acm, range) => acm + range.size }
    }.sum

    val cellsWithoutTrack = m * n - cellsWithTrack

    println(cellsWithoutTrack)
  }

  private[this] implicit final class RangeExtension[T: Integral](range: NumericRange[T]) {

    import Ordering.Implicits._

    def includes(rhs: NumericRange[T]): Boolean = {
      range.start <= rhs.start && rhs.end <= range.end
    }
  }

  private[this] implicit final class RangeListExtension[T: Integral](list: Seq[NumericRange[T]]) {
    def collapse: Seq[NumericRange[T]] =
      list.sortBy(_.start)
        .foldLeft(List.empty[NumericRange[T]]) { (accumulator, range) =>
          assume(
            range.step == implicitly[Integral[T]].one,
            "Inorder to collapse al list of ranges, all ranges must have step size one"
          )

          accumulator match {
            //completely contained => don't do anything
            case head :: tail if head.includes(range) => head :: tail
            //start contained, end not => expand head with new end
            case head :: tail if head.contains(range.start) =>
              NumericRange.inclusive(head.start, range.end, implicitly[Integral[T]].one) :: tail
            //completely uncontained => add unaltered
            case _ => range :: accumulator
          }
        }
  }

  private[this] final class Track[T: Integral](x: (T, NumericRange[T])) {
    def row: T = x._1

    def start: T = x._2.start

    def end: T = x._2.end
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