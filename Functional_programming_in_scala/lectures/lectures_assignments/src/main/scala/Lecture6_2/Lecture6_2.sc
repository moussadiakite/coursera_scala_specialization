
object exercise{
  def isPrime(n: Int):Boolean = (2 until n) forall (x => n % x != 0)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{case (x, y) => x * y}.sum

  val n = 7
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter{case (x, y) => isPrime(x + y)}

  for{
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (for{
      (x, y) <- xs zip ys
    } yield x * y).sum

  scalarProduct(Vector(1, 2, 3), Vector(4, 5, 6))
  scalarProduct2(Vector(1, 2, 3), Vector(4, 5, 6))
}