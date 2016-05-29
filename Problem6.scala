import scala.annotation.tailrec
def task6 = {
    val max = 100
    @tailrec
    def sumSquares(max: Long, acc: Long = 0): Long =
        if (max == 0) acc
        else sumSquares(max - 1, max * max + acc)
        
    @tailrec
    def sum(maxValue: Long, acc: Long = 0): Long =
        if (maxValue <= 0) acc
        else sum(maxValue - 1, maxValue + acc)
    
    sum(max) * sum(max) - sumSquares(max)
}
print(task6)
