import java.math.BigDecimal
import scala.annotation.tailrec

val one = new BigDecimal(1)
val two = new BigDecimal(2)
val degree = 1000 

@tailrec
def pow(num: BigDecimal, degree: Long, acc: BigDecimal = one): BigDecimal = {
  if (degree < 0) throw new RuntimeException("Only positive degree")
  else if (degree == 0) acc
  else pow(num, degree - 1, acc.multiply(num))
}

val result = pow(two, degree)
  .toPlainString
  .split("")
  .map(_.toLong)
  .foldLeft(0L)(_ + _)
print(result)
