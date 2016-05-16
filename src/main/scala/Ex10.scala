import utils.{Primes, Timing}

/**
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

    Find the sum of all the primes below two million.
  */
object Ex10 extends App {

  Timing.chrono(_ => {
    val primes = Timing.chrono[Seq[Int]](_ => Primes.allUnder(2000000))

    val result = primes.map(_.toLong).sum

    println(result)
  })

}
