object exercise{
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y::ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y::ys => y::init(ys)
  }

  def concat[T](xs: List[T], ys:List[T]): List[T] = xs match {
    case List() => ys
    case z::zs => z::concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y::ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => xs
    case y::ys => if(n == 0) ys else y::removeAt(n - 1, ys)
  }

  def removeAt2[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

  removeAt(1, List('a', 'b', 'c', 'd'))

  removeAt2(1, List('a', 'b', 'c', 'd'))

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case y::ys => y match {
      case List() => flatten(ys)
      case z::zs => flatten(z::zs) ++ flatten(ys)
      case _ => y::flatten(ys)
    }
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}