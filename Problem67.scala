import scala.io.Source

object Problem67 {
  def main(args: Array[String]): Unit = {
    var triangle = List[List[Long]]()
    for (line <- Source.fromFile("Problem67.txt").getLines) {
      val rawNums = line
        .trim
        .split(" ")
        .toList
        .map(_.toLong)
      triangle = triangle :+ rawNums
    }
    triangle = triangle.reverse
    for (i <- 0 to triangle.size - 2) {
      var newNextLayer = List[Long]()
      val layer = triangle(i)
      val nextLayer = triangle(i + 1)
      for (j <- 0 to layer.size - 2) {
        val num = layer(j)
        val nextNum = layer(j + 1)
        val max =
          if (num > nextNum) num
          else nextNum
        newNextLayer = newNextLayer :+ (max + nextLayer(j))
      }
      triangle = triangle.updated(i + 1, newNextLayer)
    }
    println(triangle.last.head)
  }
}

Problem67.main(Array[String]())
