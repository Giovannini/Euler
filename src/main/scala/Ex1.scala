/**
  * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  * Find the sum of all the multiples of 3 or 5 below 1000.
  */
object Ex1 extends App {

  val exResult = (1 until 10).filter(a => a % 3 == 0 || a % 5 == 0).sum
  println(exResult)

  val result = (1 until 1000).filter(a => a % 3 == 0 || a % 5 == 0).sum
  println(result)

}
