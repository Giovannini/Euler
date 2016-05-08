/**
  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143 ?
  */
object Ex3 extends App {

  def isAPrimeFactor(n: Long, a: Int) = n % a == 0

  def getPrimeFactors(n: Long, acc: Seq[Int] = Nil): Seq[Int] = {
    if (n > 1) {
      val sqrtN: Double = Math.sqrt(n)
      val maybeFactor = (2 to Math.round(sqrtN).toInt).find(isAPrimeFactor(n, _))
      maybeFactor match {
        case Some(factor) => getPrimeFactors(n / factor, factor +: acc)
        case None => n.toInt +: acc
      }
    } else acc

  }

  getPrimeFactors(600851475143L).headOption.foreach(println)

}
