/**
  * The following iterative sequence is defined for the set of positive integers:
  *
  * n → n/2 (n is even)
  * n → 3n + 1 (n is odd)
  *
  * Using the rule above and starting with 13, we generate the following sequence:
  *
  * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
  * Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
  *
  * Which starting number, under one million, produces the longest chain?
  *
  * NOTE: Once the chain starts the terms are allowed to go above one million.
  */
object Ex14 extends App {

  def computeCollatzSequenceSize(n: Long, acc: Seq[Long] = Nil, knownSequences: Map[Long, Long]): Long = {
    if (n <= 1) (acc :+ n).length
    else {
      val maybeAlreadyKnownSequence = knownSequences.get(n)
      maybeAlreadyKnownSequence match {
        case Some(seq) => acc.length + seq
        case None =>
          val nextValue = if (n % 2 == 0) n / 2 else 3 * n + 1
          computeCollatzSequenceSize(nextValue, acc :+ n, knownSequences)
      }
    }
  }

  def buildMappingsFromSequence(collatzSequence: Seq[Long], mappings: Map[Long, Long])
  : Map[Long, Long] = collatzSequence match {
    case Nil => mappings
    case xs => buildMappingsFromSequence(xs.tail, mappings + (xs.head -> xs.length))
  }

  def getLongestChain(testedNumbers: Seq[Long], currentLongestChain: (Long, Long), knownSequences: Map[Long, Long])
  : Long = testedNumbers match {
    case Seq() => currentLongestChain._1
    case head :: tail =>
      val currentChainLength: Long = computeCollatzSequenceSize(head, Nil, knownSequences)
      val newLongestChain: (Long, Long) =
        if (currentChainLength > currentLongestChain._2) (head, currentChainLength)
        else currentLongestChain
      getLongestChain(tail, newLongestChain, knownSequences + (head -> currentChainLength))
  }

  val result = getLongestChain((2L to 1000000L).toList.reverse, (1, 1), Map((1, 1)))

  println(result)

}
