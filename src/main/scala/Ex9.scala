/**
    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b2 = c^2
    For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
  */
object Ex9 extends App {

  val result = for {
    a <- 1 to 998
    b <- a to 998
    c <- b to 998
    if a + b + c == 1000
    if a * a + b * b == c * c
  } yield a * b * c

  println(result)

}
