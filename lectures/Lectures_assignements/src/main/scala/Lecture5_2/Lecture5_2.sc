object exercise{
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2;
    if(n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (List(), _) => ys
        case (_, List()) => xs
        case (z::zs, a::as) => if(ord.lt(z, a)) z :: merge(zs, ys) else a :: merge(xs, as)
      }
      val (fst, snd) = xs.splitAt(n) // fst = first and snd = second
      merge(msort(fst)(ord), msort(snd)(ord))
    }
  }

  val nums = List(5, -4, 2, 7, 1)
  msort[Int](nums)
}