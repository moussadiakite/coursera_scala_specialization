import math.abs

object exercise{
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double): Boolean =
    math.abs((x - y) / x) < tolerance

  def averageDamp(f: Double => Double)(y: Double): Double = (y + f(y)) / 2

  def fixedPoint(f: Double => Double)(firstGuessDouble:Double): Double ={
    def iterate(guess: Double): Double =
    {
      val next = averageDamp(f)(guess)
      if(isCloseEnough(next, guess)) guess
      else iterate(next)
    }
    iterate(firstGuessDouble)
  }

  def sqrt(x: Double): Double = fixedPoint(y => x / y)(1)
  sqrt(2)
}