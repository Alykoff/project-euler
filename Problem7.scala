import scala.annotation.tailrec
def task7 = {
    val primeNum = 10001
    
    @tailrec
    def isPrime(value: Int, count: Int = 2): Boolean = {
        val sqrt = math.sqrt(value).toInt
        if (sqrt < count) true
        else if (value % count == 0) false
        else isPrime(value, count + 1)
    }
    
    @tailrec
    def getPrime(count: Int = 2, primeCount: Int = primeNum): Int = {
        if (primeCount == 0) count - 1
        else getPrime(count + 1, if (isPrime(count)) primeCount - 1 else primeCount)
    }
    getPrime()
}
print(task7)
