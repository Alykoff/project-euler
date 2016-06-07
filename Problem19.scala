import java.time.DayOfWeek

import scala.collection.immutable.TreeSet

/*
You are given the following information, but you may prefer to do some research for yourself.

. 1 Jan 1900 was a Monday.
. Thirty days has September (9), April (4), June (6) and November (11).
  All the rest have thirty-one (1,2,5,7,8,10,12),
  Saving February alone,Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
. A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */

var acc = 0
val monthsWith31 = TreeSet(1, 3, 5, 7, 8, 10, 12)
val monthsWith30 = TreeSet(4, 6, 9, 11)

def nextDayOfWeek(dayOfWeek: Int): Int =
  if (dayOfWeek == 7) 1
  else dayOfWeek + 1

def isLeap(year: Int): Boolean =
  year % 4 == 0 && (year % 100 != 0 || (year % 100 == 0 && year % 400 == 0))

def next(day: Int, month: Int, year: Int): (Int, Int, Int) =
  if (day < 28) (day + 1, month, year)
  else if (monthsWith31.contains(month) && day == 31) {
    if (month == 12) (1, 1, year + 1)
    else (1, month + 1, year)
  } else if (monthsWith30.contains(month) && day == 30) {
    (1, month + 1, year)
  } else if (month == 2) {
    val isLeapYear = isLeap(year)
    if (day == 29) (1, month + 1, year)
    else if (isLeapYear) (29, month, year)
    else (1, month + 1, year)
  } else (day + 1, month, year)

def visitor(day: Int, month: Int, year: Int, dayOfWeek: Int) = {
  if (year > 1900 && dayOfWeek == 7 && day == 1) {
    acc += 1
  }
}

var day = 1
var month = 1
var year = 1900
var dayOfWeek = 1

while (day != 1 || month != 1 || year != 2001) {
  visitor(day, month, year, dayOfWeek)
  dayOfWeek = nextDayOfWeek(dayOfWeek)
  val date = next(day, month, year)
  day = date._1
  month = date._2
  year = date._3
}

println(acc)