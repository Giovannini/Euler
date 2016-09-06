import scala.io.Source

object Ex67 extends App {

  val triangle = Source.fromFile("src/main/resources/p067_triangle.txt")
    .getLines()
      .map(_.split(" ").map(_.toInt).toSeq)
      .toSeq

  val result = Ex18.getBestSum(triangle)

  println(result)

}
