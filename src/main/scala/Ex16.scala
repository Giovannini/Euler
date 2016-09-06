/**
  * 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  * What is the sum of the digits of the number 2^1000?
  */
object Ex16 extends App {

  // 2, 4, 8,  7,  5, 10,  11, 13
  // 2, 4, 8, 16, 32, 64, 128, 256

  def doubleThatSeq(seq: Seq[Int]) = {
    def doubleThatSeqReq(reverseSeq: Seq[Int], acc: Seq[Int], retenue: Boolean): Seq[Int] = reverseSeq match {
      case Nil => {if(retenue) Seq(1) else Nil} ++ acc
      case head :: tail =>
        val double = head * 2 + (if (retenue) 1 else 0)
        if (double > 9) doubleThatSeqReq(tail, (double - 10) +: acc, retenue = true)
        else doubleThatSeqReq(tail, double +: acc, retenue = false)
    }

    doubleThatSeqReq(seq.reverse, Nil, retenue = false)
  }

  def sumFor2Power(n: Int) = {
    def sumFor2PowerRec(n: Int, acc: Seq[Int]): Seq[Int] = n match {
      case 1 => acc
      case other if other > 1 =>
        sumFor2PowerRec(n - 1, doubleThatSeq(acc))
    }

    n match {
      case a if a == 1 => 2
      case a if a < 1 => 0
      case a => sumFor2PowerRec(a, Seq(2)).sum
    }
  }

  val test = sumFor2Power(15)
  val result = sumFor2Power(1000)

  println(test)
  println(result)

}
