package HackerRank.WeekOfCode30

/**
  * @author A. Roberto Fischer <a.robertofischer@gmail.com> on 3/17/2017
  */
object E {

  import java.util.concurrent.TimeUnit

  import scala.util.Random

  def main(args: Array[String]): Unit = {

    def modEquality(input: Array[Int], left: Int, right: Int, x: Int, y: Int): Int = {
      var result = 0
      for (i <- left to right) {
        if (input(i) % x == y) result += 1
      }
      result
    }

    def sort(tuple: (Int, Int)): (Int, Int) = {
      if (tuple._1 < tuple._2) tuple else tuple.swap
    }


    val a = new Array[Int](40000)
    for (l <- 0 until 40000) {
      a(l) = (40000 * Random.nextDouble()).toInt
    }

    val yx = new Array[(Int, Int)](40000)
    for (j <- 0 until 40000) {
      yx(j) = sort(((40000 * Random.nextDouble()).toInt, (40000 * Random.nextDouble()).toInt))
    }

    //val leftRight = new Array[(Int, Int)](40000)
    //for (k <- 0 until 40000) {
    //  leftRight(k) = sort(((39999 * Random.nextDouble()).toInt, (39999 * Random.nextDouble()).toInt))
    //}
    //
    //val t0 = System.nanoTime()
    //for (i <- 1 to 40000) {
    //  val (y, x) = yx(i)
    //  val (left, right) = leftRight(i)
    //  print(modEquality(a, left, right, x, y), "")
    //}
    //val t1 = System.nanoTime()
    //println(t1 - t0)
    //println("Elapsed time: " + TimeUnit.SECONDS.convert(t1 - t0, TimeUnit.NANOSECONDS) + "seconds")
    //println("finished")


    val leftRight = new Array[(Int, Int)](40000)
    for (k <- 0 until 40000) {
      leftRight(k) = (0, 39999)
    }

    val t0 = System.nanoTime()
    for (i <- 0 until 40000) {
      val (y, x) = yx(i)
      val (left, right) = leftRight(i)
      println(modEquality(a, left, right, x, y))
    }
    val t1 = System.nanoTime()
    println(t1 - t0)
    println("Elapsed time: " + TimeUnit.SECONDS.convert(t1 - t0, TimeUnit.NANOSECONDS) + "seconds")
    println("finished")
  }
}
