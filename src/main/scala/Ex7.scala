import utils.Primes

object Ex7 extends App {
  val result = Primes.stream.take(10001).toList.last

  println(result)
}
