/**
  * You are given the following information, but you may prefer to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
  * April, June and November.
  * All the rest have thirty-one,
  * Saving February alone,
  * Which has twenty-eight, rain or shine.
  * And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
  *
  * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
  */
object Ex19 extends App {

  Map(
    (1, 1, 1900) -> "Monday"
  )

  val allDates = for {
    year <- 1900 to 2000
    month <- 1 to 12
    day <- {
      if (month == 2) 1 to (if (year % 4 == 0) 29 else 28)
      else if (Seq(4, 6, 9, 11).contains(month)) 1 to 30
      else 1 to 31
    }
  } yield (day, month, year)

  val result = allDates
    .zipWithIndex
    .map { case (date, index) => (date, index % 7) }
    .count { case (date, dayOfWeek) =>
      date._3 != 1900 && // 1900 is not in the desired century
        date._1 == 1 && // Only firsts of month
        dayOfWeek == 6
    }

  println(result)

}
