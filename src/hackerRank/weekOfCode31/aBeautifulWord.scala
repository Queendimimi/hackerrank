package hackerRank.weekOfCode31

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
object aBeautifulWord {
  private val INPUT = ""

  def solve(): Unit = {
    val word = nextString
    // Print 'Yes' if the word is beautiful or 'No' if it is not.
    val vowels = Set('a', 'e', 'i', 'o', 'u', 'y')
    if (word.length > 1) {
      val isBeautiful = word.sliding(2, 1).forall { pair =>
        val a = pair(0)
        val b = pair(1)

        a != b && (!vowels.contains(a) || !vowels.contains(b))
      }
      if (isBeautiful) out.println("Yes") else out.println("No")
    } else {
      out.println("Yes")
    }
  }

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
    var i = 0
    while (i < n) {
      a(i) = nextInt()
      i += 1
      i - 1
    }
    a
  }

  private def nextLongArray(n: Int): Array[Long] = {
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = nextLong()
      i += 1
      i - 1
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


