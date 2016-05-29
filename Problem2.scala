import scala.annotation.tailrec
def task2 = {
    val maxValue = 4000000
    val fibs = scala.collection.mutable.Map(1L -> 1L, 2L -> 2L)
    def fib(x: Long): Long = {
        if (x < fibs.size) fibs(x)
        else if (x == 1 || x == 2) x
        else {
            val newFib = fib(x - 1) + fib(x - 2)
            fibs += (x -> newFib)
            newFib
        }
    }
    var i = 1
    while(fib(i) < maxValue) {
        i = i + 1
    }
    fibs.filter(x => x._2 < maxValue && x._2 % 2 == 0).map(_._2).sum
}

def task2_ver2 = {
    val maxValue = 4000000
    @tailrec
    def sum(first: Int, second: Int, acc: Int = 0): Int = {
        if (second >= maxValue) acc
        else sum (
            second, 
            first + second, 
            if (second % 2 == 0) acc + second else acc
        )
    }
    sum(1, 2)
}
print(task2)
