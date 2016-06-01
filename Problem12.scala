import scala.math._
import scala.annotation.tailrec
val numOfDivisors = 500

@tailrec
def findTriangleWithDivisors(num: Long = 1, nextNatural: Long = 2, numOfDiv: Long): Long = {
  val currentDivisors = findNumOfDivisors(num)
  if (currentDivisors < numOfDiv) {
    findTriangleWithDivisors(num + nextNatural, nextNatural + 1, numOfDiv) 
  } else {
    num
  }
}

def findNumOfDivisors(num: Long): Long = {
  @tailrec
  def findNumOfDivisorsHelper(i: Long = 1, acc: Long = 0): Long = {
    if (i >= num) acc + 1
    else if (num % i == 0) findNumOfDivisorsHelper(i + 1, acc + 1)
    else findNumOfDivisorsHelper(i + 1, acc)
  }
  findNumOfDivisorsHelper()
}
print(findTriangleWithDivisors(numOfDiv = numOfDivisors))
