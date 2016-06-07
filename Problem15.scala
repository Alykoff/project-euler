import scala.annotation.tailrec
/*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
there are exactly 6 routes to the bottom right corner.
How many such routes are there through a 20×20 grid?
 */

// Number of routes is binomial coefficient
@tailrec
def factorial(n: BigDecimal, acc: BigDecimal = BigDecimal(1)): BigDecimal =
  if (n == 1L) acc
  else factorial(n - 1, n * acc)

def binCoefficient(n: BigDecimal, k: BigDecimal): BigDecimal =
  factorial(n) / (factorial(k) * factorial(n - k))

print(binCoefficient(40, 20).toLong)