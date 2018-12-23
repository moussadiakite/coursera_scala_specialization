object NewtonSqrt {
  def sqrt(x: Double): Double = {
    def improve(guess: Double) = {
      (x / guess + guess) / 2
    }

    def isGoodEnough(guess: Double): Boolean = {
      Math.abs(guess * guess - x) / x < 0.001
    }

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    sqrtIter(1)
  }

  sqrt(1e50)
}