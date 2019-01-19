object exercise{
  def sum(xs: Array[Int]): Int = {
    xs.par.fold(0)(_ + _)
  }

  def max(xs: Array[Int]): Int = {
    xs.par.fold(Int.MinValue)(_ max _)
  }
}