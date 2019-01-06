object primes{
  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val nats = from(0)

  val m4s = nats map (_ * 4)

  val primes = sieve(from(2))

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    Math.abs(guess * guess - x) / x < 0.001
  }

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double): Double = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  sqrtStream(4) filter(isGoodEnough(_, 4)) take 10 toList
}