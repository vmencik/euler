package cz.vmencik.euler

object Problem12 extends App {
  val triangles = new Iterator[Int] {
    var sum = 0
    var i = 0
    def hasNext = true
    def next = {
      i += 1
      sum += i
      sum
    }
  }

  def countDivisors(t: Int) =
    Range(1, t+1)
      .takeWhile(n => n * n <= t)
      .foldLeft(0)((s, n) => if (t % n == 0) s + 2 else s)
  
  val r = triangles.find(countDivisors(_) >= 500)
  println(r)
}