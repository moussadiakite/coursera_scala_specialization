package running_computetions_in_parallel

import common.common._

object running_computetions_in_parallel extends App {

  def power(x: Int, p: Double): Int = math.exp(p * math.log(math.abs(x))).toInt

  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    var i = s
    var sum: Int = 0
    while (i < t){
      sum += power(a(i), p)
      i = i + 1
    }
    sum
  }

  var treshold = 4

  def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    if(t - s < treshold){
      sumSegment(a, p, s, t)
    } else {
      val m = s + (t - s) / 2
      val (sum1, sum2) = parallel(segmentRec(a, p, s, m),
                                  segmentRec(a, p, m, t))
      sum1 + sum2
    }
  }

  def pNorm(a: Array[Int], p: Double): Int =
    power(segmentRec(a, p, 0, a.length), 1 / p)

  val l = -2 to 2
  println(l.toList)
}
