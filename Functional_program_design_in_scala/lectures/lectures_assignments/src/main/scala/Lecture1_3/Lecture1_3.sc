object exercise{
  trait Generator[+T]{
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate).generate
    }
  }

  val integers: Generator[Int] = new Generator[Int] {
    val rand = new java.util.Random
    override def generate: Int = rand.nextInt()
  }

  val booleans = for(x <- integers) yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for{
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  def choose[T](lo: Int, hi: Int): Generator[Int] =
    for(x <- integers) yield lo + x % (hi - lo)


  def oneOf[T](xs: T*): Generator[T] =
    for(idx <- choose(0, xs.length)) yield xs(idx)

  def lists: Generator[List[Int]] = for{
    isEmpty <- booleans
    list <- if(isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)
  def nonEmptyLists = for{
    head <- integers
    tail <- lists
  } yield head::tail

  lists.generate

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for{
    isLeaf <- booleans
    tree <- if(isLeaf) leafNode else innerNode
  } yield tree

  def leafNode = for(x <- integers) yield Leaf(x)
  def innerNode = for{
    left <- trees
    right <- trees
  } yield Inner(left, right)

  trees.generate

  def test[T](g: Generator[T], numTimes: Int = 100)
             (test: T => Boolean): Unit = {
    for(_ <- 0 until numTimes){
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed" + numTimes + "tests")
  }

  test(pairs(lists, lists)){
    case (xs, ys) => (xs ++ ys).length > xs.length
  }
}