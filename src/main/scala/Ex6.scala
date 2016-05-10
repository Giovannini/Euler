object Ex6 extends App{

  val sumSquare = (1 to 100).map(BigDecimal(_)).map(b => b * b).sum

  val sum = BigDecimal((1 to 100).sum)
  val squareSum = sum * sum

  println(squareSum - sumSquare)

}
