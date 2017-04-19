package hackerRank.weekOfCode31

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, Growable}
import scala.io.Source
import scala.language.higherKinds

/**
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  * THE SOFTWARE.
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/18/2017
  */
object dSpanningTreeFraction {
  //  private val INPUT = "10 20\n0 8 98 2\n0 1 99 1\n1 2 100 2\n0 9 99 1\n7 8 98 1\n2 8 100 2\n3 5 100 2\n3 3 97 2\n0 3 100 1\n1 4 98 1\n1 8 98 1\n7 9 97 1\n6 9 100 1\n4 5 100 2\n3 9 98 2\n7 8 99 2\n3 6 98 1\n0 5 100 2\n2 5 99 1\n5 7 99 1\n0"

  private val source = Source.fromURL("https://hr-testcases-us-east-1.s3.amazonaws.com/36646/input11.txt?AWSAccessKeyId=AKIAJAMR4KJHHUS76CYQ&Expires=1492650408&Signature=g%2BH7aDNrJ4yCDZVv3ZFaIXBPsnQ%3D&response-content-type=text%2Fplain")
  private val INPUT = source.getLines mkString "\n"

  //------------------------------------------------------------------------------------------//
  // SOLUTION
  //------------------------------------------------------------------------------------------//
  var n = 0
  var m = 0

  private def solve(): Unit = {
    n = nextInt()
    m = nextInt()
    val edges = nextSeq[Edge, Vector]({
      val u = nextInt()
      val v = nextInt()
      val a = nextInt()
      val b = nextInt()
      Vector(Edge(u, v, a, b), Edge(v, u, a, b))
    }, m)
    val optimalMSTSum = fractionalLinearOptimization2(edges, 0, 10000001)
    val divider = gcd(optimalMSTSum._1.toInt, optimalMSTSum._2.toInt)
    out.println(optimalMSTSum._1.toInt / divider + "/" + optimalMSTSum._2.toInt / divider)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  private def fractionalLinearOptimization(edges: Seq[Edge], left: Int, right: Int) = {
    @tailrec
    def _binarySearch(prev: Either[(Double, Double), (Double, Double)],
                      left: Double,
                      right: Double): (Double, Double) = {
      val factor = (left + right) / 2.0
      val current = improveTarget(edges, factor)
      if (prev.isRight && current.isRight && prev.right.get == current.right.get) {
        current.right.get
      } else {
        current match {
          case Left(_) => _binarySearch(current, left, factor)
          case Right(_) => _binarySearch(current, factor, right)
        }
      }
    }

    _binarySearch(Left((0.0, 0.0)), left, right)
  }

  private def fractionalLinearOptimization2(edges: Seq[Edge], left: Int, right: Int) = {

    @tailrec
    def _binarySearch(prevFactor: Double, left: Double, right: Double): (Double, Double) = {
      val factor = (left + right) / 2.0
      val target = improveTarget(edges, factor)

      if (Math.abs(prevFactor - factor) < 0.0001) {
        target.fold(identity, identity)
      } else {
        target match {
          case Left(_) => _binarySearch(factor, left, factor)
          case Right(_) => _binarySearch(factor, factor, right)
        }
      }
    }

    _binarySearch(0.0, left, right)
  }


  private def fractionalLinearOptimization3(targetFunction: => Either[(Double, Double), (Double, Double)],
                                            epsilon: Double,
                                            left: Int,
                                            right: Int) = {
    @tailrec
    def _binarySearch(prevFactor: Double, left: Double, right: Double): (Double, Double) = {
      val factor = (left + right) / 2.0
      val target = targetFunction
      if (Math.abs(prevFactor - factor) < 0.0001) {
        target.fold(identity, identity)
      } else {
        target match {
          case Left(_) => _binarySearch(factor, left, factor)
          case Right(_) => _binarySearch(factor, factor, right)
        }
      }
    }

    _binarySearch(0.0, left, right)
  }


  private def improveTarget(edges: Seq[Edge], factor: Double) = {
    val (aSum, bSum) = kruskalMST(edges, factor)
    val improvement = aSum >= bSum * factor
    if (improvement) Right((aSum, bSum)) else Left((aSum, bSum))
  }

  private def kruskalMST(edges: Seq[Edge], factor: Double) = {
    val unionFind = UnionFind(n)
    val edgeList = edges.sortBy(_.weight(factor))
    var aSum = 0.0
    var bSum = 0.0
    for (edge <- edgeList) {
      val u = edge.u
      val v = edge.v
      if (unionFind(u) != unionFind(v)) {
        unionFind.union(u, v)
        aSum += edge.a
        bSum += edge.b
      }
    }
    (aSum, bSum)
  }

  case class Edge(u: Int, v: Int, a: Int, b: Int) {
    def weight(factor: Double): Double = {
      -(a - b * factor)
    }
  }

  case class UnionFind(size: Int, lazyConstruct: Boolean = false)
    extends PartialFunction[Int, Int] with Growable[Int] {
    private[this] type Rank = Int
    private[this] type Node = Int
    private[this] type Root = Int

    private[this] var parent = new Array[Node](size)
    private[this] var rank = new Array[Rank](size)

    if (!lazyConstruct) {
      for (i <- 0 until size) {
        +=(i)
      }
    }

    override def apply(x: Node): Root = {
      find(x)
    }

    override def clear(): Unit = {
      parent = new Array[Node](size)
      rank = new Array[Rank](size)
    }

    override def isDefinedAt(x: Int): Boolean = x < parent.length && 0 <= x

    override def +=(v: Node): UnionFind.this.type = {
      parent(v) = v
      rank(v) = 0
      this
    }

    private[this] def find(v: Node): Root = {
      if (v == parent(v)) {
        v
      } else {
        parent(v) = find(parent(v))
        parent(v)
      }
    }

    def sets: Map[Int, Iterable[Int]] = {
      (0 until size).groupBy(find)
    }

    def normalUnion(a: Node, b: Node): Unit = {
      parent(find(a)) = find(b)
    }

    def randomizedUnion(a: Node, b: Node): Unit = {
      // Randomized linking is O(an) too: http://www.cis.upenn.edu/~sanjeev/papers/soda14_disjoint_set_union.pdf
      if (scala.util.Random.nextBoolean()) {
        parent(find(a)) = find(b)
      } else {
        parent(find(b)) = find(a)
      }
    }

    //Union with path compression by rank
    def union(a: Node, b: Node): Unit = {
      var aRoot = find(a)
      var bRoot = find(b)

      if (aRoot != bRoot) {
        if (rank(aRoot) < rank(bRoot)) {
          val temp = aRoot
          aRoot = bRoot
          bRoot = temp
        }
        parent(bRoot) = aRoot
        if (rank(aRoot) == rank(bRoot)) {
          rank(aRoot) = rank(aRoot) + 1
        }
      }
    }
  }

  //------------------------------------------------------------------------------------------//
  // Input-Output                                                                 
  //------------------------------------------------------------------------------------------//
  var in: java.io.InputStream = _
  var out: java.io.PrintWriter = _

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    run()
  }

  @throws[Exception]
  def run(): Unit = {
    in = if (INPUT.isEmpty) System.in else new ByteArrayInputStream(INPUT.getBytes)
    out = new PrintWriter(System.out)

    val s = System.currentTimeMillis
    solve()
    out.flush()
    if (!INPUT.isEmpty) print(System.currentTimeMillis - s + "ms")
  }

  private val inputBuffer = new Array[Byte](1024)
  var lenBuffer = 0
  var ptrBuffer = 0

  private def readByte(): Int = {
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

  private def isSpaceChar(c: Int) = !(c >= 33 && c <= 126)

  private def skip = {
    var b = 0
    while ( {
      b = readByte()
      b != -1 && isSpaceChar(b)
    }) {}
    b
  }

  private def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
    }
    builder.result()
  }

  private def nextSeq[T, Coll[_]](reader: => Seq[T], n: Int)
                                 (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder ++= reader
    }
    builder.result()
  }

  private def nextDouble[Coll[Double]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Double], Double, Coll[Double]]): Coll[Double] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextDouble()
    }
    builder.result()
  }

  private def nextChar[Coll[Char]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Char], Char, Coll[Char]]): Coll[Char] = {
    val builder = cbf()
    builder.sizeHint(n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      builder += b.toChar
      p += 1
      b = readByte()
    }
    builder.result()
  }

  private def nextMultiLine(n: Int, m: Int): Array[Array[Char]] = {
    val map = new Array[Array[Char]](n)
    var i = 0
    while (i < n) {
      map(i) = nextChar[Array](m)
      i += 1
    }
    map
  }

  private def nextInt[Coll[Int]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
    }
    builder.result()
  }

  private def nextLong[Coll[Long]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Long], Long, Coll[Long]]): Coll[Long] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextLong()
    }
    builder.result()
  }

  private def nextDouble(): Double = nextString.toDouble

  private def nextChar: Char = skip.toChar

  private def nextString: String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) {
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
  }

  private def nextInt(): Int = {
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

  private def nextLong(): Long = {
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
    throw new IOException("Read Long")
  }

  private def print(o: AnyRef*): Unit = {
    System.out.println(java.util.Arrays.deepToString(o.toArray)
    )
  }
}