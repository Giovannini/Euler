package utils

object Timing {


  /**
    * Measure the time it takes to run a computation
    * Utilization:
    * Timing.chrono(_ => {
    *   val result = Primes.stream.takeWhile(_ < 2000000).toList.map(_.toLong).sum
    *
    *   println(result)
    * })
    *
    * @param computation the computation to run
    */
  def chrono[A](computation: Unit => A) = {
    val t1 = System.currentTimeMillis()

    val result: A = computation()

    val t2 = System.currentTimeMillis()
    println("This computation took " + (t2 - t1) + "ms.")
    result
  }

}
