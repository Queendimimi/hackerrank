val sc = new java.util.Scanner(System.in)
val time = sc.next()
val pmOrAm = time takeRight 2
val hoursMinutesSeconds = time.dropRight(2).split(":").map(_.trim)
pmOrAm match {
  case "AM" =>
    val adjustedHours = hoursMinutesSeconds(0) match {
      case "12" => "00"
      case _ => hoursMinutesSeconds(0)
    }
    println(s"$adjustedHours:${hoursMinutesSeconds(1)}:${hoursMinutesSeconds(2)}")
  case "PM" =>
    val adjustedHours = hoursMinutesSeconds(0) match {
      case "12" => "12"
      case _ => hoursMinutesSeconds(0).toInt + 12
    }
    println(s"$adjustedHours:${hoursMinutesSeconds(1)}:${hoursMinutesSeconds(2)}")
  case _ => println("Input format error")
}


