import scala.annotation.tailrec
def task10 = {
    val maxNum = 2000000
    
    @tailrec
    def isPrime(value: Int, count: Int = 2): Boolean = {
        val sqrt = math.sqrt(value).toInt
        if (sqrt < count) true
        else if (value % count == 0) false
        else isPrime(value, count + 1)
    }
    
    @tailrec
    def getPrimeSum(count: Int = 2, acc: Long = 0): Long = {
        if (count > maxNum) acc
        else {
            val newAcc = 
                if (isPrime(count)) count + acc
                else acc
            getPrimeSum(count + 1, newAcc)
        }
    }
    getPrimeSum()
}
print(task10)
