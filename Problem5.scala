import scala.annotation.tailrec
def task5 = {
    val maxValue = 20
    
    @tailrec
    def maxPow(value: Int, pow: Int = 0): Int =
        if (math.pow(value, pow) > maxValue) pow - 1
        else maxPow(value, pow + 1)
    
    @tailrec
    def isPrime(value: Int, count: Int = 2): Boolean = {
        val sqrt = math.sqrt(value).toInt
        if (sqrt < count) true
        else if (value % count == 0) false
        else isPrime(value, count + 1)
    }
    (2 to maxValue).filter(isPrime(_)).foldLeft(1) {(x: Int, y: Int) =>
        math.pow(y, maxPow(y)).toInt * x
    }
}
print(task5)
