package cz.vmencik.euler

import scala.Array.canBuildFrom
import scala.io.Source
import java.util.Calendar

object Problem17 extends App {

  val lowNums = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five",
    6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 11 -> "eleven", 10 -> "ten",
    12 -> "twleve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen",
    17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen").mapValues(_.length)

  val tens = Map(1 -> "ten", 2 -> "twenty", 3 -> "thirty", 4 -> "forty", 5 -> "fifty",
    6 -> "sixty", 7 -> "seventy", 8 -> "eighty", 9 -> "ninety").mapValues(_.length)

  val hundred = "hundred".length
  
  val countChars: Int => Int = {
    case n if (n < 20) => lowNums(n)
    case n if (n < 100) =>
      tens(n / 10) + (if (n % 10 > 0) lowNums(n % 10) else 0)
    case n if (n < 1000) =>
      countChars(n / 100) + hundred + (if (n % 100 > 0) 3 + countChars(n % 100) else 0)
    case 1000 => 11
  }
  
  val r = (1 to 1000).map(countChars).sum
  println(r)
}

object Problem18And67 extends App {
  lazy val data: IndexedSeq[Array[Int]] = {
     val src = Source.fromInputStream(getClass.getResourceAsStream("data18.txt")) // replace with data67.txt for Problem 67
     val iter = for (line <- src.getLines) yield (line.split("\\s+").map(_.toInt))
     iter.toIndexedSeq
  }
  
  val start = System.currentTimeMillis
  val totals = (0 until data.length).foldLeft(IndexedSeq[Int]()) { (lastLineTotals, i) =>
    for (j <- 0 until data(i).length) yield {
      val v = data(i)(j)
      val totalUpLeft = if (j - 1 >= 0) v + lastLineTotals(j - 1) else v
      val totalUpRight = if (j < lastLineTotals.length) v + lastLineTotals(j) else v
      math.max(totalUpLeft, totalUpRight)
    }
  }
  
  println(totals.max)
  
}

object Problem19 extends App {
  val c = Calendar.getInstance
  c.set(1901, Calendar.JANUARY, 1)
  
  var r = 0;
  while (c.get(Calendar.YEAR) < 2001) {
    if (c.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
      r += 1
    }
    c.add(Calendar.MONTH, 1)
  }
  println(r)
}

object Problem20 extends App {
  val fact100 = Range.BigInt.inclusive(1, 100, 1).product
  val r = fact100.toString.view.map(_.asDigit).sum
  println(r)
}