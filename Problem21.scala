import scala.annotation.tailrec
val bound = 10000
val numOfPrimes = 100
val prms = primes(numOfPrimes)

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

def calcSumOfDivisors(n: Long) = {
  if (prms.contains(n)) 1L
  else {
    var acc = 1L
    var i = 2L
    while (i < n) {
      if (n % i == 0) acc += i
      i += 1
    }
    acc
  }
}

val sumsDivs = Range(2, bound)
    .map(_.toLong)
    .map(x => (x, calcSumOfDivisors(x)))
    .filter(pair => pair._1 != pair._2 && pair._2 <= bound)
    .toMap

var result = sumsDivs.foldLeft(0L)((sum, x) =>
  if (sumsDivs.contains(x._2) && sumsDivs(x._2) == x._1) sum + x._1
  else sum
)

println(result)