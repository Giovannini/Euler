/**
  * Created by tgi on 04/09/2016.
  */
object Ex20 extends App {

  def computeFactoSum(n: Int) = {
    require(n > 1)

    val stringResult: String = (2 to n)
      .toList
        .foldLeft("1"){ case (acc, a) =>
          val (result, r) = acc
            .split("")
            .map(_.toInt)
            .foldRight(("", 0)) { case (int, (acc2, rest)) =>
              val product = int * a + rest
              ((product % 10).toString + acc2, product / 10)
            }
            if (r > 0) r.toString + result else result
        }

    println(stringResult)

    stringResult
      .split("")
      .map(_.toInt)
      .sum
  }

  val example: Int = computeFactoSum(10)
  println(example)
  val result: Int = computeFactoSum(100)

  println(result)

}
