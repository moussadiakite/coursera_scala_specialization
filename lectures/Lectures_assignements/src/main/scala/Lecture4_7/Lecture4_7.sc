object exercise{
  def insert(y: Int, sortedList: List[Int]): List[Int] = sortedList match {
    case List() => List()
    case x::xs => if(y > x) x::insert(y, xs) else y::sortedList
  }

  def iSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y::ys => insert(y, iSort(ys))
  }
}