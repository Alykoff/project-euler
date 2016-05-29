import scala.annotation.tailrec
def task4 = {
    val minVal = 100
    val maxVal = 999
    def isPalidrome(value: Int): Boolean = {
        value.toString == value.toString.reverse
    }
    @tailrec
    def grabPalindroms(x: Int = maxVal, y: Int = maxVal, acc: List[Int] = List.empty): List[Int] = {
        val nextY = if (y <= minVal) x - 1 else y - 1
        val nextX = if (y <= minVal) x - 1 else x
        val product = x * y
        
        if (nextX < minVal || nextY < minVal) acc
        else if (isPalidrome(product)) grabPalindroms(nextX, nextY, product :: acc)
        else grabPalindroms(nextX, nextY, acc)
    }
    grabPalindroms().max
}
print(task4)
