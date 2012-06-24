package cz.vmencik.euler

import scala.collection.immutable.Stream

object P1 extends App {
  val sum = (1 until 1000).view filter (i => i % 3 == 0 || i % 5 == 0) reduce (_ + _)
  println(sum)
}

object P2 extends App {
  val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)
  val sum = (fibs takeWhile (_ < 4000000) filter (_ % 2 == 0)).sum
  println(sum)
}

object P3 extends App {
  val num: Long = 600851475143L
  val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
    primes.takeWhile(j => j * j <= i).forall(i % _ > 0))

  val limit = math.sqrt(num)
  val maxPrime = primes.takeWhile(_ < limit).filter(num % _ == 0).last
  println(maxPrime)
}

object P4 extends App {
  val products = for {
    i <- 100 to 999
    j <- i to 999
    p = (i * j).toString if p.reverse == p
  } yield i * j
  println(products.max)
}

object P5 extends App {
  val x = Range(20, Int.MaxValue).find(n => Range(2, 20).forall(n % _ == 0)).get
  println(x)
}
