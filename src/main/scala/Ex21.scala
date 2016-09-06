/**
  * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called
  * amicable numbers.
  *
  * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
  * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
  *
  * Evaluate the sum of all the amicable numbers under 10000.
  */
object Ex21 extends App {

  def getDivisors(n: Int) = {
    val possibleDivisors: Seq[Int] = 1 to Math.sqrt(n).toInt
    val lowerDivisors = possibleDivisors.filter(n % _ == 0)
    lowerDivisors ++ lowerDivisors.map(n / _) diff Seq(n)
  }

  def computeResult(numbers: Seq[Int], amicableNumbers: Set[Int]): Set[Int] = numbers match {
    case Nil => amicableNumbers
    case a +: as =>
      val dOfA: Int = getDivisors(a).sum
      val dOfB: Int = getDivisors(dOfA).sum

      if(dOfB == a && dOfA != a) computeResult(as, amicableNumbers ++ Set(dOfA, dOfB))
      else computeResult(as, amicableNumbers)
  }

  val result = computeResult((1 until 10000).toSeq, Set())
  println(result)
  println(result.sum)

}
