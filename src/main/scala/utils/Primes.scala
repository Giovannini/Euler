package utils

object Primes {

  def stream: Stream[Int] = {
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

  /**
    * Get all primes number under a certain value (higher than 10)
    * @param n the maximum prime number
    * @return
    */
  def allUnder(n: Int): Seq[Int] = {
    val initialPrimes = List(2, 3, 5, 7)
    runSieve((11 to n).filter(x => initialPrimes.forall(p => x % p != 0)).toList, initialPrimes)
  }

  def runSieve(possiblyPrimes: List[Int], primes: List[Int]): List[Int] = possiblyPrimes match {
    case Nil => primes
    case newPrime :: tail => runSieve(tail.filter(_ % newPrime != 0), newPrime :: primes)
  }

}
