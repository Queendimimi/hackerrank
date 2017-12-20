import scala.collection.SortedMap

val months = SortedMap(
  1 -> 31,
  2 -> 28,
  3 -> 31,
  4 -> 30,
  5 -> 31,
  6 -> 30,
  7 -> 31,
  8 -> 31,
  9 -> 30,
  10 -> 31,
  11 -> 30,
  12 -> 31
)

def printDateInRussia(year: Int): Unit = {
  val date = dateInRussia(year, 256)
  println(("%02d".format(date._1), "%02d".format(date._2), date._3).productIterator.mkString("."))
}

def dateInRussia(year: Int, dayOfTheYear: Int): (Int, Int, Int) = {
  def daysAndMonth(monthsMap: SortedMap[Int, Int]): (Int, Int) = {
    monthsMap.values.toStream
      .scanLeft(0)(_ + _)
      .zip(Stream from 1)
      .takeWhile(_._1 < dayOfTheYear)
      .last
  }

  year match {
    case 1918 =>
      val dayAndMonth = daysAndMonth(gregorianYear(year) + (2 -> 15))
      if (dayAndMonth._2 == 2) {
        (dayOfTheYear - dayAndMonth._1 + 13, dayAndMonth._2, year)
      } else {
        (dayOfTheYear - dayAndMonth._1, dayAndMonth._2, year)
      }

    case _ if year > 1918 =>
      val dayAndMonth = daysAndMonth(gregorianYear(year))
      (dayOfTheYear - dayAndMonth._1, dayAndMonth._2, year)

    case _ if year < 1918 =>
      val dayAndMonth = daysAndMonth(julianYear(year))
      (dayOfTheYear - dayAndMonth._1, dayAndMonth._2, year)
  }
}

def gregorianYear(year: Int): SortedMap[Int, Int] = {
  if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) {
    months + (2 -> 29)
  } else {
    months + (2 -> 28)
  }
}

def julianYear(year: Int): SortedMap[Int, Int] = {
  if (year % 4 == 0) {
    months + (2 -> 29)
  } else {
    months + (2 -> 28)
  }
}

println(printDateInRussia(1918))
println(dateInRussia(1918, 31))
println(dateInRussia(1918, 32))
println(dateInRussia(1918, 45))
println(dateInRussia(1918, 46))
println(dateInRussia(1918, 47))
println(printDateInRussia(2016))
println(printDateInRussia(2017))

