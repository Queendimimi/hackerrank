package HackerRank

import scala.collection.generic.{CanBuildFrom, Clearable}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{Iterator, mutable}
import scala.util.{Failure, Success, Try}

/**
  * Index is hashCode based, no hashcode equal objects are permitted.
  *
  * @see [[https://algs4.cs.princeton.edu/24pq/IndexMinPQ.java.html]]
  */

object IndexedPriorityQueue {

  class MinPriorityQueue[A](nodes: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty[A],
                            indices: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int])
                           (implicit val ord: Ordering[A])
    extends Serializable
      with mutable.Iterable[A]
      with Clearable
      with Cloneable {

    import ord._

    override def isEmpty: Boolean = {
      nodes.isEmpty
    }

    override def size: Int = {
      nodes.size
    }

    private def insert(key: Int, value: A): Try[Unit] = {
      indices.get(key).fold[Try[Unit]] {
        nodes += value
        indices(key) = lastIndex()
        val u: Unit = fixUp(lastIndex(), value)
        Success(u)
      }(_ => Failure(new IllegalArgumentException(s"$key is already present")))
    }

    def enqueue(value: A): Try[Unit] = {
      insert(value.hashCode(), value)
    }

    override def head: A = {
      if (nonEmpty) {
        nodes(firstIndex())
      } else {
        throw new NoSuchElementException("queue is empty")
      }
    }

    def dequeue(): Option[A] = {
      if (nonEmpty) {
        val minNode = nodes(firstIndex())

        if (size > 1) {
          swap(nodes(firstIndex()), nodes(lastIndex()))
        }

        nodes.remove(lastIndex())
        indices.remove(minNode.hashCode())


        if (size > 1) {
          fixDown(firstIndex(), nodes(firstIndex()))
        }

        Option(minNode)
      } else {
        None
      }
    }

    def dequeueAll[A1 >: A, That](implicit bf: CanBuildFrom[_, A1, That]): That = {
      val b = bf.apply()
      while (nonEmpty) {
        b += dequeue().get
      }
      b.result()
    }

    private def changeKey(key: Int, value: A, heapify: (Int, A) => Unit): Try[Unit] = {
      for {
        _ <- indices.get(value.hashCode()).fold[Try[Unit]](Success(()))(_ => Failure(new
            IllegalArgumentException(s"$value already present")))
        result <- indices.get(key).fold[Try[Unit]](
          Failure(new NoSuchElementException(s"$key is not present"))
        ) { i =>
          nodes(i) = value
          indices.remove(key)
          indices(value.hashCode()) = i
          heapify(i, value)
          Success(())
        }
      } yield result
    }

    private def decreaseKey(key: Int, value: A): Try[Unit] = {
      changeKey(key, value, fixUp)
    }

    private def increaseKey(key: Int, value: A): Try[Unit] = {
      changeKey(key, value, fixDown)
    }

    def decreasePriority(oldValue: A, newValue: A): Try[Unit] = {
      Try(require(oldValue >= newValue, "priority must be decreased"))
        .flatMap(_ => decreaseKey(oldValue.hashCode(), newValue))
    }

    def increasePriority(oldValue: A, newValue: A): Try[Unit] = {
      Try(require(oldValue <= newValue, "priority must be increased"))
        .flatMap(_ => increaseKey(oldValue.hashCode(), newValue))
    }

    def changePriority(oldValue: A, newValue: A): Try[Unit] = {
      if (oldValue <= newValue) {
        increaseKey(oldValue.hashCode(), newValue)
      } else {
        decreaseKey(oldValue.hashCode(), newValue)
      }
    }

    @inline private def parentIndex(index: Int) = {
      (index - 1) / 2
    }


    @inline private def leftChildIndex(index: Int) = {
      (2 * index) + 1
    }

    @inline private def rightChildIndex(index: Int) = {
      (2 * index) + 2
    }


    @inline private def firstIndex() = {
      0
    }

    @inline private def lastIndex() = {
      nodes.size - 1
    }

    private def fixUp(lastIndex: Int, value: A): Unit = {
      if (size > 1) {
        var i = lastIndex

        while (i > 0 & value < nodes(parentIndex(i))) {
          if (nodes(i) < nodes(parentIndex(i))) {
            swap(nodes(i), nodes(parentIndex(i)))
          }
          i = parentIndex(i)
        }
      }
    }

    private def fixDown(firstIndex: Int, value: A): Unit = {
      if (size > 1) {
        var i = firstIndex

        while (i < lastIndex() && minChild(i).fold(false)(value > _)) {

          val minNode = minChild(i).get
          val minIndex = indices(minNode.hashCode())

          if (nodes(i) > minNode) {
            swap(nodes(i), nodes(minIndex))
          }

          i = minIndex
        }
      }
    }

    private def minChild(index: Int): Option[A] = {
      if (leftChildIndex(index) <= size && rightChildIndex(index) <= size) {
        if (rightChildIndex(index) == size) {
          Some(nodes(leftChildIndex(index)))
        } else if (nodes(leftChildIndex(index)) < nodes(rightChildIndex(index))) {
          Some(nodes(leftChildIndex(index)))
        } else {
          Some(nodes(rightChildIndex(index)))
        }
      } else {
        None
      }
    }

    private def swap(nodeX: A, nodeY: A): Unit = {
      val tempNode = nodes(indices(nodeY.hashCode))
      nodes(indices(nodeY.hashCode)) = nodes(indices(nodeX.hashCode))
      nodes(indices(nodeX.hashCode)) = tempNode

      val tempIndex = indices(nodeY.hashCode)
      indices(nodeY.hashCode) = indices(nodeX.hashCode)
      indices(nodeX.hashCode) = tempIndex
    }


    override def clear(): Unit = {
      nodes.clear()
      indices.clear()
    }

    override def clone(): MinPriorityQueue[A] = {
      new MinPriorityQueue[A](nodes.clone(), indices.clone())
    }

    override def iterator: Iterator[A] = {
      nodes.toIterator
    }

    override def toStream: Stream[A] = {
      this.clone().dequeueAll.toStream
    }

    override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = {
      val b = cbf()
      b ++= this.clone().dequeueAll
      b.result()
    }

    override def toString: String = {
      var first = true
      val start = stringPrefix + "("
      val sep = ", "
      val end = ")"
      val builder = new StringBuilder()
      builder append start
      for (x <- this.clone().dequeueAll) {
        if (first) {
          builder append x
          first = false
        }
        else {
          builder append sep
          builder append x
        }
      }
      builder append end

      builder.toString()
    }

  }

  object MinPriorityQueue {
    def apply[A](nodes: ArrayBuffer[A] = mutable.ArrayBuffer.empty[A],
                 indices: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int])
                (implicit ord: Ordering[A]): MinPriorityQueue[A] = {
      new MinPriorityQueue(nodes, indices)(ord)
    }
  }

  final class MaxPriorityQueue[A](nodes: ArrayBuffer[A] = mutable.ArrayBuffer.empty[A],
                                  indices: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int])
                                 (implicit ord: Ordering[A])
    extends MinPriorityQueue(nodes, indices)(ord.reverse) {
    override def decreasePriority(oldValue: A, newValue: A): Try[Unit] = {
      super.increasePriority(oldValue, newValue)
    }

    override def increasePriority(oldValue: A, newValue: A): Try[Unit] = {
      super.decreasePriority(oldValue, newValue)
    }
  }

  object MaxPriorityQueue {
    def apply[A](nodes: ArrayBuffer[A] = mutable.ArrayBuffer.empty[A],
                 indices: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int])
                (implicit ord: Ordering[A]): MaxPriorityQueue[A] = {
      new MaxPriorityQueue(nodes, indices)(ord)
    }
  }

}

