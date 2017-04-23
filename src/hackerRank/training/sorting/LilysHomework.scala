package hackerRank.training.sorting

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util.InputMismatchException

import scala.collection.generic.CanBuildFrom
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
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/20/2017
  */
object LilysHomework {
  private val INPUT = "274\n656085744 592976686 3037922 82266352 17574000 344340000 8406892 591292449 569625472 899357375 251327440 301303036 281400020 77370228 15516426 82859300 88364436 39767760 148417500 306863056 10926174 118195200 310408774 309307894 200852782 82193280 424056750 249277128 180368880 477624006 86748948 7434336 48882310 112635040 6614541 503907132 598034610 160500171 70444416 72752680 271416096 30521205 529365648 399367584 129849984 263500556 76737948 464269640 613416088 162724716 163420800 720512988 1217212920 727647624 383190420 8350904 3456024 289141064 123384024 158867856 82005504 36225521 533012608 54370440 17671500 53627000 26597644 855638940 55343960 57828624 108025344 21431808 1182600 265643950 30054300 219553915 74203500 658061160 7502400 931691763 295769136 107345925 80109000 130922055 33544944 65280 452996453 301655430 7828912 425016000 297635898 26861016 739961600 928116 19645470 8691456 5123880 596100015 2735436 25596483 173620720 202797504 161748972 30122300 11082820 574006860 426732182 71136825 105659136 1808140278 450779280 286831620 104683008 938781480 175050736 255681692 67096152 119518575 15449840 25273458 165048960 5642130 85199958 354920488 446786340 131214816 41533296 25766518 90782304 59007600 700369740 122021794 56982366 238027920 434370816 223677580 72463156 355858300 144914616 145950 13822570 19914930 11072100 21450528 124958730 105156800 20843784 781192188 448358850 6139822 95694780 78713888 677177472 9963972 366432768 181113408 725292862 473528052 12864000 518355540 55832070 318508876 89963781 1796290329 844308846 428627693 276255100 123609720 440449488 27589680 426614166 110068200 408846096 620309228 186565236 49051552 561738897 650114105 32646556 7174400 275364045 945364797 3674160 66314292 11073770 14885370 245088324 669628848 33110250 971699976 324099072 259496060 492110802 52206516 508725376 1534995792 148078816 57993375 121071195 381960195 12496176 23728250 159836835 712982980 160098622 909675852 110400300 485423372 30637838 339925836 285371600 13618242 80809650 92375040 265612788 1151909241 234661320 16962144 213417000 269646860 1015650090 117439476 53164566 6946134 89506800 305469360 13406432 292353000 9969642 43982198 23887296 67730660 16384192 218824704 1082660306 1679473908 136336179 39265884 1077096020 464272368 87048192 56752487 156212388 360621525 8472816 17613600 62143172 82696127 79939536 155805468 159499963 262072360 39827904 179532360 2455040 327280740 148409340 73738980 3394872 3082560 225009981 188912256 179487784 340340 315171072 96680664 621206280 536379504 391714074 65055960 105570465 365721408 106712025 286764660\n0"

  //------------------------------------------------------------------------------------------//
  // Solution                                                                
  //------------------------------------------------------------------------------------------//
  private def solve(): Unit = {
    val n = nextInt()
    val input = nextIntWithIndex[Vector](n).sortBy(_._1)
    out.println(Math.min(countSwaps(input), countSwaps(input.reverse)))
  }

  private def countSwaps(input: Seq[(Int, Int)]) = {
    val visited = Array.fill[Boolean](input.length)(false)
    var swaps = 0
    for (i <- input.indices) {
      if (i == input(i)._2 || visited(i)) {
        visited(i) = true
      } else {
        var cycle = i
        var cycleLength = 0
        while (!visited(cycle)) {
          cycleLength += 1
          visited(cycle) = true
          cycle = input(cycle)._2
        }
        swaps += cycleLength - 1
      }
    }
    swaps
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
    if (!INPUT.isEmpty) printCustom(System.currentTimeMillis - s + "ms")
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

  private def next[T, Coll[_]](reader: => T, n: Int)
                              (implicit cbf: CanBuildFrom[Coll[T], T, Coll[T]]): Coll[T] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += reader
    }
    builder.result()
  }

  private def nextWithIndex[T, Coll[_]](reader: => T, n: Int)
                                       (implicit cbf: CanBuildFrom[Coll[(T, Int)], (T, Int), Coll[(T, Int)]]): Coll[(T, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((reader, i))
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

  private def nextDoubleWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Double, Int)], (Double, Int), Coll[(Double, Int)]]): Coll[(Double, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextDouble(), i))
    }
    builder.result()
  }

  private def nextChar[Coll[_]]
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

  private def nextCharWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Char, Int)], (Char, Int), Coll[(Char, Int)]]): Coll[(Char, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      builder += ((b.toChar, p))
      p += 1
      b = readByte()
    }
    builder.result()
  }

  private def nextInt[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Int], Int, Coll[Int]]): Coll[Int] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextInt()
    }
    builder.result()
  }

  private def nextIntWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Int, Int)], (Int, Int), Coll[(Int, Int)]]): Coll[(Int, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextInt(), i))
    }
    builder.result()
  }

  private def nextLong[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[Long], Long, Coll[Long]]): Coll[Long] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (_ <- 0 until n) {
      builder += nextLong()
    }
    builder.result()
  }

  private def nextLongWithIndex[Coll[_]]
  (n: Int)(implicit cbf: CanBuildFrom[Coll[(Long, Int)], (Long, Int), Coll[(Long, Int)]]): Coll[(Long, Int)] = {
    val builder = cbf()
    builder.sizeHint(n)
    for (i <- 0 until n) {
      builder += ((nextLong(), i))
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

  private def nextDouble(): Double = nextString().toDouble

  private def nextChar(): Char = skip.toChar

  private def nextString(): String = {
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

  private def printCustom(o: AnyRef*): Unit = {
    System.out.println(java.util.Arrays.deepToString(o.toArray)
    )
  }
}