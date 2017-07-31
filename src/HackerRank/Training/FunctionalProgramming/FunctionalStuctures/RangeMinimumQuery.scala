package HackerRank.Training.FunctionalProgramming.FunctionalStuctures

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.control.TailCalls._

/**
  * Copyright (c) 2017 A. Roberto Fischer
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 6/16/2017
  */
private[this] object RangeMinimumQuery {
  private[this] val INPUT = ""

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private[this] def solve(): Unit = {
    val n = nextInt()
    val m = nextInt()

    val input = nextInt[Vector](n)

    val segmentationTree = SegmentationTree[Int](Int.MaxValue, input, Math.min)

    next[Unit, Vector]({
      println(segmentationTree.query(nextInt(), nextInt()).result)
    }, m)
  }

  abstract class Node[T]

  case class InnerNode[T](l: Int, r: Int, value: T, leftChild: Node[T], rightChild: Node[T]) extends Node[T]

  case class Leaf[T](idx: Int, value: T) extends Node[T]

  final case class SegmentationTree[T](default: T, points: Seq[T], f: (T, T) => T) {
    val root: Node[T] = initTree(buildTree(0, points.size - 1, default).result, points, f)

    //------------------------------------------------------------------------------------------//
    // Public
    //------------------------------------------------------------------------------------------//
    def query(i: Int, j: Int, rootNode: Node[T] = root, g: (T, T) => T = f): TailRec[T] = {
      rootNode match {
        case InnerNode(l, r, value, leftChild, rightChild) =>
          if (l == i & j == r) {
            done(value)
          }
          else if (j <= mid(l, r)) {
            tailcall(query(i, j, leftChild, g))
          }
          else if (i > mid(l, r)) {
            tailcall(query(i, j, rightChild, g))
          }
          else {
            done(g(
              tailcall(query(i, mid(l, r), leftChild, g)).result,
              tailcall(query(mid(l, r) + 1, j, rightChild, g)).result
            ))
          }

        case Leaf(_, value) => done(value)
      }
    }

    //------------------------------------------------------------------------------------------//
    // Private
    //------------------------------------------------------------------------------------------//
    private[this] def mid(l: Int, r: Int): Int = (l + r) / 2

    private[this] def initTree(rootNode: Node[T], points: Seq[T], f: (T, T) => T): Node[T] = {
      var root = rootNode
      for ((p, idx) <- points.zipWithIndex) {
        root = insertPoint(root, idx, p, f).result._1
      }
      root
    }

    private[this] def buildTree(l: Int, r: Int, default: T): TailRec[Node[T]] = {
      if (l != r)
        done(InnerNode(
          l,
          r,
          default,
          tailcall(buildTree(l, mid(l, r), default)).result,
          tailcall(buildTree(mid(l, r) + 1, r, default)).result
        ))
      else
        done(Leaf(l, default))
    }

    private[this] def insertPoint(rootNode: Node[T],
                            idx: Int,
                            value: T,
                            g: (T, T) => T): TailRec[(Node[T], T)] = {
      rootNode match {
        case InnerNode(l, r, prevValue, leftChild, rightChild) =>
          if (idx <= mid(l, r)) {
            val (updatedLeft, leftValue) = tailcall(insertPoint(leftChild, idx, value, g)).result
            val aggValue = g(prevValue, leftValue)
            done(InnerNode(l, r, aggValue, updatedLeft, rightChild), aggValue)
          }
          else {
            val (updatedRight, rightValue) = tailcall(insertPoint(rightChild, idx, value, g)).result
            val aggValue = g(prevValue, rightValue)
            done(InnerNode(l, r, aggValue, leftChild, updatedRight), g(prevValue, aggValue))
          }

        case Leaf(i, _) => done((Leaf(i, value), value))
      }
    }
  }

  //------------------------------------------------------------------------------------------//
  // Input-Output                                                                 
  //------------------------------------------------------------------------------------------//
  private[this] var in: java.io.InputStream = _
  private[this] var out: java.io.PrintWriter = _

  private[this] def println(x: Any) = out.println(x)

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    run()
  }

  @throws[Exception]
  private[this] def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) System.out.println(System.currentTimeMillis - s + "ms")
  }

  private[this] def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
    }
    builder.result()
  }

  private[this] def nextInt[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
    }
    builder.result()
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

  private[this] val inputBuffer = new Array[Byte](1024)
  private[this] var lenBuffer = 0
  private[this] var ptrBuffer = 0

  private[this] def readByte(): Int = {
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

}