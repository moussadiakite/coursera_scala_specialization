object exercise {
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) ((x, y) => f(x) :: y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((x, y) => y + 1)

  val nums = List(5, -4, 2, 7, 1)

  def f(x: Int) = x * x

  mapFun[Int, Int](nums, x => {
    x * x
  })

  lengthFun(nums)
}