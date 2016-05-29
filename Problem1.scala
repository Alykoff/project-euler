import scala.annotation.tailrec
import scala.math

def task1 = {
    val maxValue = 1000
    @tailrec
    def sum(count: Int, acc: Int = 0): Int = {
        if (count >= maxValue) acc
        else if (count % 3 == 0 || count % 5 == 0) sum(count + 1, acc + count)
        else sum(count + 1, acc)
    }
    sum(1)
}

def task1_ver2 = {
    (1 until 1000).foldLeft(0)((x: Int, y: Int) => {
        if (y % 3 == 0 || y % 5 == 0) x + y
        else x
    })
}
print(task1)
