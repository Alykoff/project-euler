import scala.annotation.tailrec
val numOfDivisors = 500
val numOfPrimes = 1000
val prms = primes(numOfPrimes)

// n-th triangular number m: m = n * (n - 1) / 2
// for odd n: D(m) = D(n) * D((n - 1) / 2)
// for even n: D(m) = D(n / 2) * D(n - 1)
// where D(x) - is function return number of factors.

def isPrime(num: Long): Boolean = {
  if (num == 1) false
  else if (num < 4) true
  else if (num % 2 == 0) false
  else if (num < 9) true
  else if (num % 3 == 0) false
  else {
    val root = scala.math.floor(scala.math.sqrt(num)).toLong
    var fraction = 5L
    while (fraction <= root && num % fraction != 0 && num % (fraction + 2) != 0) {
      fraction += 6
    }
    if (num % fraction == 0 || num % (fraction + 2) == 0) false
    else true
  }
}

@tailrec
def primes(max: Long, i: Int = 1, acc: List[Long] = List(2L)): List[Long] = {
  if (i >= max) acc
  else if (isPrime(i)) primes(max, i + 2, acc :+ i.toLong)
  else primes(max, i + 2, acc)
}

// num -> D(num)
var cash = scala.collection.mutable.Map(
  1L -> 1L, 
  2L -> 2L,
  3L -> 2L, 
  6L -> 4L, 
  10L -> 4L, 
  15L -> 4L, 
  21L -> 4L,
  28L -> 6L
)

def calcDn(n: Long) = {
  var varN = n
  if (cash.contains(n)) cash(n)
  else {
    var dN = 1L
    var i = 0
    var isBreak = false
    while (i < prms.size && !isBreak) {
      val iPrime = prms(i)
      if (iPrime * iPrime > varN) {
        dN *= 2
        isBreak = true
      } else {
        var exponent = 1L
        while (varN % iPrime == 0) {
          exponent += 1L
          varN /= iPrime
        }
        if (exponent > 1) dN *= exponent
        if (varN == 1) {
          isBreak = true
        }
      }
      if (!isBreak) {
        i += 1
      }
    }
    cash(n) = dN
    dN
  }
}

var n = 3L // start with second triangular number
var cnt = 0L
while (cnt <= numOfDivisors) {
  n += 1
  val isN1Even = n % 2 == 0
  cnt = 
    if (isN1Even) {
      calcDn(n / 2) * calcDn(n - 1)
    } else {
      calcDn(n) * calcDn((n - 1) / 2)
    }
  cash(n * (n - 1) / 2) = cnt
}
println(n * (n - 1) / 2)