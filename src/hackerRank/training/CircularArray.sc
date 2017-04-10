import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, reflectiveCalls}


implicit class IterableExt[A, Col](xs: Col)
                                  (implicit c2s: Col => Seq[A],
                                   cbf: CanBuildFrom[Col, A, Col]) {
  def rotateRight(i: Int): Col = {
    val builder = cbf()
    val size = xs.size
    builder ++= xs.view.drop(size - (i % size)) ++ xs.view.take(size - (i % size))
    builder.result()
  }

  def rotateLeft(i: Int): Col = {
    val builder = cbf()
    val size = xs.size
    builder ++= xs.view.drop(i % size) ++ xs.view.take(i % size)
    builder.result()

  }
}


val a = List(1, 2, 3)

a.drop(1) ++ a.take(1) foreach println


a.rotateLeft(1) foreach println

a.rotateRight(1) foreach println