import scala.annotation.tailrec

val maxValue = 1000 * 1000

@tailrec
def calcSeq(num: Long, acc: Long = 1): Long = {
    if (num == 1) acc
    else if (num % 2 == 0) calcSeq(num / 2, acc + 1)
    else calcSeq(3 * num + 1, acc + 1)
}

var max = 1L
var maxSeed = 1L
for (i <- 1 to maxValue if i % 2 != 0) {
    val res = calcSeq(i) 
    if (res > max) {
        max = res
        maxSeed = i
    }
}

if (maxSeed % 2 == 0 && maxSeed * 2 <= maxValue) {
    maxSeed *= 2
    max += 1
}

println("maxSeed: " +  maxSeed)
println("seq len: " + max)
