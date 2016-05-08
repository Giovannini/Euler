/**
  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers
  is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  */
object Ex4 extends App {

  val products = for {
    x <- 100 to 999
    y <- 100 to 999
  } yield (x * y, x, y)

  val (product, x, y) = products
    .reverse
    .filter{ case (p, _, _) => p.toString == p.toString.reverse }
    .maxBy{ case (p, _, _) => p }
  println(s"Largest palindrome found: $product, product of $x and $y.")



}
