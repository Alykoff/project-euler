import scala.annotation.tailrec

/*
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant
if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of
two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123
can be written as the sum of two abundant numbers.
However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number
that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
val bound = 28123
val numOfPrimes = math.floor(math.sqrt(bound)).toLong
val primes = getPrimes(numOfPrimes)

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
def getPrimes(max: Long, i: Int = 1, acc: List[Long] = List(2L)): List[Long] = {
  if (i >= max) acc
  else if (isPrime(i)) getPrimes(max, i + 2, acc :+ i.toLong)
  else getPrimes(max, i + 2, acc)
}

// better use this fact:
// http://mathschallenge.net/index.php?section=faq&ref=number/sum_of_divisors
def calcSumOfDivisors(n: Long) = {
  if (primes.contains(n)) 1L
  else {
    val isEven = n % 2 == 0
    val step =
      if (isEven) 1L
      else 2L
    var acc = 1L
    var i =
      if (isEven) 2L
      else 3L
    val root = math.floor(math.sqrt(n)).toLong
    while (i <= root) {
      if (n % i == 0) acc += i + n / i
      i += step
    }
    acc
  }
}

// TODO