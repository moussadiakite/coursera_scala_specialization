object exercise{
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y::ys => (y * y)::squareList(ys)
  }

  def squareListMap(xs: List[Int]): List[Int] = xs map (x => x * x)

  val nums = List(5, -4, 2, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
    }
  }

  def encodeHelper[T](xs: List[List[T]]): List[(T, Int)] = xs match {
    case List() => List()
    case x::xs => (x.head, x.length)::encodeHelper(xs)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = encodeHelper(pack(xs))

  def encode2[T](xs: List[T]): List[(T, Int)] = pack(xs) map (x => (x.head, x.length))

  pack(List("a", "a", "a", "b", "c", "c", "a"))
  encode(List("a", "a", "a", "b", "c", "c", "a"))
  encode2(List("a", "a", "a", "b", "c", "c", "a"))
}