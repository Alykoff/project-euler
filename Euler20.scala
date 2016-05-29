package org.eiler

import scala.annotation.tailrec
object Euler20 {
  def task11 = {
    val in = """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
						49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
						81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
						52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
						22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
						24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
						32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
						67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
						24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
						21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
						78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
						16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
						86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
						19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
						04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
						88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
						04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
						20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
						20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
						01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""

    val rawMatrix =
      in.split("[\n\t]")
        .toList
        .filterNot(_.isEmpty)
    val len = rawMatrix.size
    val lenSeq = 4
    def transpose(xs: List[List[Int]]) = {
      val size: Int = xs.size
      val empty = (0 to size).map(x => List.empty[Int]).toList
      xs.foldLeft(empty) { (acc: List[List[Int]], y: List[Int]) =>
        (for (i <- 0 to size - 1) yield {
          y(i) :: acc(i)
        }).toList
      }
    }
    @tailrec
    def calcInDiagonal(xs: List[List[Int]], row: Int, column: Int, count: Int = 0, acc: Long = 1L): Long = {
      if (count == lenSeq) acc
      else if (xs(column)(row) == 0) 0
      else calcInDiagonal(xs, row + 1, column + 1, count + 1, xs(column)(row) * acc)
    }

    @tailrec
    def produceOnDiagonal(xs: List[List[Int]], row: Int = 0, column: Int = len, acc: List[Long] = List.empty): List[Long] = {
      if (row == len && column == 0) acc
      else if (column + lenSeq > len || row + lenSeq > len) {
        val newJ = if (column > row) column - row - 1 else 0
        val newI = if (column > row) 0 else row - column + 1
        produceOnDiagonal(xs, newI, newJ, acc)
      } else produceOnDiagonal(xs, row + 1, column + 1, calcInDiagonal(xs, row, column) :: acc)
    }

    @tailrec
    def calcInRow(xs: List[List[Int]], row: Int, column: Int, count: Int = 0, acc: Long = 1L): Long = {
      if (count == lenSeq) acc
      else if (xs(column)(row) == 0) 0
      else calcInRow(xs, row + 1, column, count + 1, xs(column)(row) * acc)
    }

    @tailrec
    def produceOnRow(xs: List[List[Int]], row: Int = 0, column: Int = 0, acc: List[Long] = List.empty): List[Long] = {
      if (row + lenSeq == len && column == len - 1) acc
      else if (row + lenSeq > len) produceOnRow(xs, 0, column + 1, acc)
      else produceOnRow(xs, row + 1, column, calcInRow(xs, row, column) :: acc)
    }

    val matrix =
      rawMatrix
        .map(
          _.trim.split(" ").toList.map(_.toInt)
        )
    val transposeMatrix = transpose(matrix)

    (produceOnDiagonal(matrix) ++
      produceOnDiagonal(transposeMatrix) ++
      produceOnRow(matrix) ++
      produceOnRow(transposeMatrix)
    ).max

  }
  def task12 = {
    def factorial(n: Int): Long = {
      val MAX_VALUE = 20
      val values: Array[Long] = new Array(MAX_VALUE)
      values.update(0, 1)
      values.update(1, 1)
      def factorialHelper(n: Int): Long = {
        if (n > MAX_VALUE) throw new RuntimeException("So big n")
        if (values(n) != 0) values(n)
        else {
          val result = n * factorialHelper(n - 1)
          values.update(n, result)
          result
        }
      }
      factorialHelper(n)
    }
    /**
     * Return value Newton binom.
     */
    def getBinom(n: Int, k: Int): Long = {
      factorial(n) / (factorial(k) * factorial(n - k))
    }
    
    def getAllBinomValue(k: Int, n: Int, acc: Long = 0L): Long =
      if (k == 0) acc
      else getAllBinomValue(k - 1, n, getBinom(n, k) + acc)

    /**
     * Return all combination without repeat numbers.
     * (1,2,3,3,3,4) return number of combination (1,2,3,4).
     */
    def getFirstSet(elements: List[(Int, Int)]): Long = {
      val numOfElements = elements.size
      getAllBinomValue(numOfElements, numOfElements)
    }

    def getSecondSet(elements: List[(Int, Int)]): Long = {
      val n = elements.size
      val secondEl = elements.filter(_._2 > 1).map {case (x, y) =>
        val maxNumOfX = y
        val minNumOfX = 2
        val newN = n - minNumOfX
//        val new
        0L
      }
      
      def getAllK(k: Int, n: Int, acc: Long = 0L): Long = {
        
        
        0L
      }
      0L
    }

    println(factorial(20))
    //    @tailrec
    //    def ll(x: Long, count: Long = 2, acc: List[Long] = List.empty): List[Long] = {
    //      if (x == 1) acc
    //      else if (x % count == 0) {
    //        ll(x / count, count, count :: acc)
    //      } else ll(x, count + 1, acc)
    //    }
    //    def ll2(x: Long, count: Long = 2, countDelemeters: Long = 0): Long = {
    //      if (x <= count) return countDelemeters + 1;
    //      else if (x % count == 0) return ll2(x, count + 1, countDelemeters + 1);
    //      else return ll2(x, count + 1, countDelemeters);
    //    }
    //
    //    val numRaw = List(19, 17, 11, 7, 5, 3, 3, 2, 2, 2, 2).foldLeft(1L)(_ * _)
    //    val num = numRaw
    //    var i = num
    //    while (i > math.sqrt(num)) {
    //      if (i % 100000000L == 0)
    //        println(i)
    //      if (num == (i + 1) * i / 2) println("********************* " + i)
    //      i = i - 1
    //    }
    //    i = 17907120L

    //		while (i < 97907120L) {
    //			
    //		}

    //		println(ll(24885214433964885L))
    //		val maxValue = 1000000
    //		var max = 0L
    //		var i = 3
    //		while(i < maxValue) {
    //			val n = (i + 1) * i / 2
    //			val result = ll2(n)
    //			if (result > max) {
    //				max = result
    //				println(n + ", size = " + result + ", " + ll(n))
    //			}
    //			i = i + 1
    //		}
    //		var acc = List.empty[Int]
    //		for (x <- 10 to Int.MaxValue - 4 if (x % 2 != 0)) {
    //			val size = ll(x/2).size
    //			if (size == 1 || size == 2 || size == 3 || size == 4) {
    //				val newEl = (x + 1)
    //				if (ll(newEl).size + size >= 9) {
    //					acc = x :: acc
    //					if (acc.size == 1000) {
    //						println(acc.sorted)
    //						exit
    //					}
    ////					println(s"x = $x, newEl = $newEl")
    //				}
    //			}
    //		}
    //		var n = 4
    //		while (ll((n + 1) * n / 2).size != 500) {
    //			if (n % 1000 == 0) println(ll((n + 1) * n / 2).size)
    //			n = n + 1
    //		}
    //		println(n)
    //		val el1 = if (n % 2 == 0) n / 2 else n
    //		val el2 = if (n % 2 == 0) (n + 1) else (n + 1) / 2
    //		val dividersEl1 = ll(el1)
    //		val dividersEl2 = ll(el2)
    //		val intersect = dividersEl1.intersect(dividersEl2)
    //		dividersEl1.size + dividersEl2.size - intersect.size == 500
    //		math.
    //		while(ll(i + 1) * ll(i) * 0.5 != 500) {
    //			println(ll(i + 1) * ll(i) * 0.5)
    //			i = i + 1
    //		}
    //		println(ll(n))
  }
}