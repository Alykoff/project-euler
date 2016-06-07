/*
What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
 */
val cash = scala.collection.mutable.Map[Long, BigDecimal](1L -> BigDecimal(1), 2L -> BigDecimal(1))

val boundNum = 1000
val boundValRaw = new StringBuilder("1")
for (i <- 1 until boundNum) boundValRaw.append("0")
val bound = BigDecimal(boundValRaw.toString)

def fib(n: Long): BigDecimal = {
  if (n == 1 || n == 2) BigDecimal(1)
  else if (cash.contains(n)) cash(n)
  else {
    val next = fib(n - 1) + fib(n - 2)
    cash(n) = next
    next
  }
}

var flagEnd = false
var n = 3L
while (!flagEnd) {
  val nextFib = fib(n)
  if (nextFib >= bound) {
    flagEnd = true
  } else {
    n += 1
  }
}

println(n)