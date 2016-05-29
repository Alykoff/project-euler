import scala.collection.mutable.MutableList
def task3 = {
    def calcFormula(checkValue: Long, formula: (Long, Long) => Long, isFull: Boolean = false): Boolean = {
        if (math.sqrt(checkValue) == math.sqrt(checkValue).toLong) false
        else {
            var count = 0
            val maxN = if (isFull) checkValue.toInt else math.sqrt(checkValue).toInt
            for (n <- 1 to maxN) {
          for (m <- 1 to math.sqrt(checkValue).toInt if (!isFull || (isFull && n > m))) {
            if (formula(n, m) == checkValue) count = count + 1
          }
        }
            count % 2 == 1
        }
    }
    
    def isPrime(value: Long): Boolean = {
        if ((value - 1) % 12 == 0) {
            calcFormula(value, (p: Long, t: Long) => {4 * p * p + t * t})
        } else if ((value - 5) % 12 == 0) {
            calcFormula(value, (p: Long, t: Long) => {4 * p * p + t * t})
        } else if ((value - 7) % 12 == 0) {
            calcFormula(value, (p: Long, t: Long) => {3 * p * p + t * t})
        } else if ((value - 11) % 12 == 0) {
            calcFormula(value, (p: Long, t: Long) => {3 * p * p - t * t}, true)
        } else {
            false
        }
    }

    def search(n: Long, acc: MutableList[Long] = MutableList.empty): MutableList[Long] = {
        for (i <- 2 to math.sqrt(n).toInt if (n % i == 0)) {
            val p = n % i
            if (i > p && isPrime(i)) acc += i
            else search(p, acc)
        }
        acc
    }
    search(600851475143L).last
}
print(task3)
