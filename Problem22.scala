import scala.io.Source

def incrementIndex[T](valueWithIndex: (T, Int)): (T, Long) =
  (valueWithIndex._1, valueWithIndex._2.toLong + 1)

val scores = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    .split("")
    .toList
    .zipWithIndex
    .map(incrementIndex)
    .toMap

val separator = ","
val lines = Source.fromFile("Problem22.txt").getLines
val line =
  if (lines.hasNext) lines.next()
  else ""

val names = line
  .replaceAll("\"", "")
  .split(separator)
  .toList
  .sorted

def calcSumOfScores(str: String): Long = {
  str
    .split("")
    .toList
    .map(x =>
      if (scores.contains(x)) scores(x)
      else 0L
    ).sum
}

val sumOfScores = names
  .map(calcSumOfScores)
  .zipWithIndex
  .map(incrementIndex)
  .map(x => x._1 * x._2)
  .sum

println(sumOfScores)