

def sortable(array: Array[Int]): Boolean = {
  for (i <- 1 to array.length) {
    val temp = array(i)
    var j = i
    while (j > 0 && array(j - 1) > temp) {
      if (swappable(array(j), array(j - 1))) {
        array(j) = array(j - 1)
        j = j - 1
      } else {
        return false
      }
    }
    array(j) = temp
  }
  true
}

def sortable2(array: Array[Int]): Boolean = {
  var max = array(0)
  for (i <- 1 until array.length) {
    val temp = array(i)
    if (max < array(i)) max = array(i)
    if (array(i - 1) > temp && swappable(array(i), array(i - 1))) {
      array(i) = array(i - 1)
      array(i - 1) = temp
    }
    if (max != array(i)) return false
  }
  true
}

def swappable(a: Int, b: Int) = {
  Math.abs(a - b) == 1
}

def main(args: Array[String]) {
  val sc = new java.util.Scanner(System.in)
  val q = sc.nextInt()
  var a0 = 0
  while (a0 < q) {
    val n = sc.nextInt()
    val a = new Array[Int](n)
    for (i <- 0 until n) {
      a(i) = sc.nextInt()
    }
    if (sortable(a)) println("Yes") else println("No")
    a0 += 1
  }
}

val test = List(1, 0, 3, 2).toArray

sortable2(test)

