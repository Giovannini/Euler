object Ex12 extends App {

  def getTriangularNumberNo(n: Int): Long = (1 to n).sum

  def getDivisors(n: Long) = {
    val sqrtN = Math.ceil(Math.sqrt(n)).toLong

    (2L to sqrtN).foldLeft(List(1L, n)) { case (divisors, testedNumber) =>
      if (n % testedNumber == 0) {
        testedNumber :: n / testedNumber :: divisors
      } else divisors
    }
  }

  val numberStream: Stream[Int] = 1 #:: numberStream.map(_ + 1)

  val triangularNumberStream: Stream[Long] = numberStream.map(getTriangularNumberNo)

  val result = triangularNumberStream.find(getDivisors(_).length > 500)

  println(result)
}
