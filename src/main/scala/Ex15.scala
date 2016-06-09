/**
    Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


    How many such routes are there through a 20×20 grid?
  */
object Ex15 extends App{

  // It's  the number of combination
  // We have to place "down"and "right" elements (20 of each) in an array of length 40 (number of total moves).
  val upperDiv = (21 to 40).toList.map(BigDecimal(_)).foldLeft(BigDecimal(1))(_ * _)
  val lowerDiv = (2 to 20).toList.map(BigDecimal(_)).foldLeft(BigDecimal(1))(_ * _)
  val result = upperDiv / lowerDiv

  println(result)

}
