object Ex7 extends App {

  def primes: Stream[Int] = {
    // The set of prime checking functions mentioned in step 1
    val checkPrimeFunctions: Seq[Int => Boolean] = Seq(_ % 2 == 0)

    def primeToPrimeChecker(prime: Int): Int => Boolean = b => b % prime == 0

    def isNotPrime(fs: Seq[Int => Boolean], num: Int): Boolean = fs.exists { f => f(num) }

    def primeStreamBuilder(num: Int, checkFunctions: Seq[Int => Boolean]): Stream[Int] = {
      // If numberToTest is not prime, go try the next number
      if (isNotPrime(checkFunctions, num)) primeStreamBuilder(num + 1, checkFunctions)
      // Else add the value to the stream and test the next value
      else num #:: primeStreamBuilder(num + 1, checkFunctions :+ primeToPrimeChecker(num))
    }

    // The infinite stream implemented with unfold
    2 #:: primeStreamBuilder(3, checkPrimeFunctions)
  }

  val result = primes.take(10001).toList.last

  println(result)

}
