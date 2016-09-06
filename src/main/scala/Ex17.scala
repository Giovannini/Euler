/**
  * If the numbers 1 to 5 are written out in words: one, two, three, four, five,
  * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be
  * used?
  *
  * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and
  * 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance
  * with British usage.
  */
object Ex17 extends App {

  val map = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine",
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen",
    20 -> "twenty",
    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "ninety"
  )

  def writeNumber(n: Int): String = map.get(n) match {
    case Some(s) => s
    case _ =>
      if (n > 999) {
        val thousandValue: String = map(n / 1000)
        s"$thousandValue thousand ${ if (n % 1000 > 0) s"and ${writeNumber(n % 1000)}" else ""}"
      } else if (n > 99) {
        val hundredValue: String = map(n / 100)
        s"$hundredValue hundred ${ if (n % 100 > 0) s"and ${writeNumber(n % 100)}" else ""}"
      } else /*if (n > 20)*/ {
        val tenthValue: String = map((n / 10) * 10)
        s"$tenthValue-${writeNumber(n % 10)}"
      }
  }

  def compute(from: Int, to: Int) = (from to to).toList
    .view
    .map(writeNumber)
    .map(_.replaceAll(" ", ""))
    .map(_.replaceAll("-", ""))
    .map(_.length)
    .force
    .sum

  val example1 = compute(1, 5)
  println(example1) // 19
  val example2 = compute(342, 342)
  println(example2) // 23
  val example3 = compute(115, 115)
  println(example3) // 20

  val result = compute(1, 1000)
  println(result)
}
