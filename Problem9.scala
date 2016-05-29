import scala.annotation.tailrec
def task9 = {
    val value = 1000
    @tailrec
    def find(c: Int = value, b: Int = value, a: Int = value): Int = {
        val newA = 
            if (b == 1) c - 1 
            else c
        val newB = 
            if (b == 1) newA
            else if (a == 1) b - 1
            else b
        val newC = 
            if (a == 1) newB
            else a - 1
            
        if ((a * a + b * b == c * c) && (c + b + a) == value) a*b*c
        else find(newA, newB, newC)
    }
    find()
}
print(task9)
