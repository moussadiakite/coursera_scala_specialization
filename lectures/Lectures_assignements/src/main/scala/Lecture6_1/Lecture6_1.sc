object exercise{
  val nonInclusive: Range = 1 until 5
  val inclusive: Range = 1 to 5

  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val s = "Progressing in scala programming"
  s filter (c => c.isUpper)

  s exists (c => c.isUpper)
  s forall (c => c.isUpper)

  val pairs = List(1, 2, 3) zip s
  pairs.unzip

  s.flatMap(c => List('*', c))

  def combinations(N: Int, M: Int): IndexedSeq[(Int, Int)] =
    (1 to N) flatMap(n => (1 to M) map (m => (n, m)))

  combinations(3, 4)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{case (x, y) => x * y}.sum

  scalarProduct(Vector(1, 2, 3), Vector(4, 5, 6))

  def isPrime(n: Int): Boolean = (2 until n) forall(x => n % x != 0)
}