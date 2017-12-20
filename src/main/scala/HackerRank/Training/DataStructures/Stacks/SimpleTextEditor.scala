package HackerRank.Training.DataStructures.Stacks

import java.io.{ByteArrayInputStream, IOException, InputStream, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 8/5/2017
*/
private[this] object SimpleTextEditor {

  import Reader._
  import Writer._

  private[this] val TEST_INPUT: Option[String] = None

  //------------------------------------------------------------------------------------------//
  // Solution
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val q = nextInt()
    val textEditor = new TextEditor()
    next[Operation with Executable, Vector]({
      nextInt() match {
        case 1 => Append(nextString())
        case 2 => Delete(nextInt())
        case 3 => Print(nextInt())
        case 4 => Undo
        case _ => throw new UnsupportedOperationException
      }
    }, q)
      .foreach(textEditor)
  }

  private[this] sealed trait Operation

  private[this] sealed trait Executable

  private[this] sealed trait Event

  private[this] sealed trait Inverse[A <: Inverse[A, B], B <: Inverse[B, A]] {
    self: A =>
    def inverse: B
  }

  private[this] final case object Undo extends Operation with Executable

  private[this] final case class Print(k: Int) extends Operation with Executable

  private[this] final case class Append(s: String) extends Operation with Executable

  private[this] final case class AppendEvent(s: String) extends Event with Inverse[AppendEvent, DeleteEvent] {
    override def inverse = DeleteEvent(s)
  }

  private[this] final case class Delete(k: Int) extends Operation with Executable

  private[this] final case class DeleteEvent(s: String) extends Event with Inverse[DeleteEvent, AppendEvent] {
    override def inverse = AppendEvent(s)
  }

  private[this] class TextEditor(opStack: mutable.ArrayStack[Inverse[_, _]] = mutable.ArrayStack[Inverse[_, _]]())
    extends (Operation with Executable => Unit) {
    private val text = mutable.ArrayStack[Char]()


    def apply(op: Operation with Executable): Unit = {
      op match {
        case Undo => undo
        case Print(k) => print(k)
        case Append(s) => append(s)
        case Delete(k) => delete(k)
      }
    }

    private[this] def undo = {
      opStack.pop().inverse match {
        case DeleteEvent(s) => (0 until s.length).foreach(_ => text.pop())
        case AppendEvent(s) => text ++= s
      }
      Unit
    }

    private[this] def print(k: Int) = {
      println(text(text.size - k))
      Unit
    }

    private[this] def append(s: String) = {
      opStack.push(AppendEvent(s))
      text ++= s
      Unit
    }

    private[this] def delete(k: Int) = {
      assume(k <= text.size)
      opStack.push(DeleteEvent(text.take(k).reverseIterator.mkString))
      (0 until k).foreach(_ => text.pop())
      Unit
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

    def nextDouble(): Double = nextString().toDouble

    def nextChar(): Char = skip.toChar

    def nextString(): String = {
      var b = skip
      val sb = new java.lang.StringBuilder
      while (!isSpaceChar(b)) {
        sb.appendCodePoint(b)
        b = readByte().toInt
      }
      sb.toString
    }

    def nextInt(): Int = {
      var num = 0
      var b = 0
      var minus = false
      while ( {
        b = readByte().toInt
        b != -1 && !((b >= '0' && b <= '9') || b == '-')
      }) {}
      if (b == '-') {
        minus = true
        b = readByte().toInt
      }
      while (true) {
        if (b >= '0' && b <= '9') {
          num = num * 10 + (b - '0')
        } else {
          if (minus) return -num else return num
        }
        b = readByte().toInt
      }
      throw new IOException("Read Int")
    }

    def nextLong(): Long = {
      var num = 0L
      var b = 0
      var minus = false
      while ( {
        b = readByte().toInt
        b != -1 && !((b >= '0' && b <= '9') || b == '-')
      }) {}
      if (b == '-') {
        minus = true
        b = readByte().toInt
      }
      while (true) {
        if (b >= '0' && b <= '9') {
          num = num * 10 + (b - '0')
        } else {
          if (minus) return -num else return num
        }
        b = readByte().toInt
      }
      throw new IOException("Read Long")
    }

    private[this] def next[T: ClassTag](): T = {
      implicitly[ClassTag[T]].runtimeClass match {
        case java.lang.Integer.TYPE => nextInt().asInstanceOf[T]
        case java.lang.Long.TYPE => nextLong().asInstanceOf[T]
        case java.lang.Double.TYPE => nextDouble().asInstanceOf[T]
        case java.lang.Character.TYPE => nextChar().asInstanceOf[T]
        case s if Class.forName("java.lang.String") == s => nextString().asInstanceOf[T]
        case _ => throw new RuntimeException("Unsupported input type.")
      }
    }

    private[this] val inputBuffer = new Array[Byte](1024)
    private[this] var lenBuffer = 0
    private[this] var ptrBuffer = 0

    private[this] def readByte()(implicit in: java.io.InputStream): Byte = {
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
        b = readByte().toInt
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