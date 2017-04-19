package hackerRank.weekOfCode30

import java.io.{ByteArrayInputStream, IOException, PrintWriter}
import java.util
import java.util.InputMismatchException

/**
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  * THE SOFTWARE.
  *
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 4/17/2017
  */
object eRangeModularQueries {
  private val INPUT = ""

  def solve(): Unit = {
    val n = nextInt()
    val q = nextInt()
    val a = nextIntArray(n)
    var a0 = 0
    while (a0 < q) {
      val left = nextInt()
      val right = nextInt()
      val x = nextInt()
      val y = nextInt()
      var result = 0
      for (i <- left to right) {
        if (a(i) % x == y) result += 1
      }
      out.println(result)
      a0 += 1
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

  private def nextDouble: Double = nextString.toDouble

  private def nextChar: Char = skip.toChar

  private def nextString: String = {
    var b = skip
    val sb = new java.lang.StringBuilder
    while (!isSpaceChar(b)) { // when nextLine, (isSpaceChar(b) && b != ' ')
      sb.appendCodePoint(b)
      b = readByte()
    }
    sb.toString
  }

  private def nextString(n: Int): Array[Char] = {
    val buf = new Array[Char](n)
    var b = skip
    var p = 0
    while (p < n && !isSpaceChar(b)) {
      buf({
        p += 1
        p - 1
      }) = b.toChar
      b = readByte()
    }
    if (n == p) buf else util.Arrays.copyOf(buf, p)
  }

  private def nextMultiLine(n: Int, m: Int): Array[Array[Char]] = {
    val map = new Array[Array[Char]](n)
    var i = 0
    while (i < n) {
      map(i) = nextString(m)
      i += 1
    }
    map
  }

  private def nextIntArray(n: Int): Array[Int] = {
    val a = new Array[Int](n)
    for (i <- 0 until n) {
      a(i) = nextInt()
    }
    a
  }

  private def nextLongArray(n: Int): Array[Long] = {
    val a = new Array[Long](n)
    for (i <- 0 until n) {
      a(i) = nextLong()
    }
    a
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
    var b = 0L
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