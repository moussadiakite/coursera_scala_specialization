
object nqueens {

  def columnsAndRowSafe(col: Int, queens: List[Int]): Boolean =
    !queens.contains(col)

  def diagonalsSafe(col: Int, queens: List[Int]): Boolean = {
    val k = queens.length
    (for{
      (i, j) <- ((k - 1) to 0 by - 1) zip queens
      if(Math.abs(col - j) == k - i)
    } yield (i, j)).isEmpty
  }

  def isSafe(col: Int, queens: List[Int]): Boolean =
    columnsAndRowSafe(col, queens) && diagonalsSafe(col, queens)

  def isSafe2(col: Int, queens: List[Int]): Boolean = {
    val k = queens.length
    val queensWithRows = ((k - 1) to 0 by - 1) zip queens
    queensWithRows.forall{
      case(r, c) => col != c && Math.abs(col - c) != (k - r)
    }
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = k match {
      case 0 => Set(List())
      case x => for{
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col::queens
    }
    placeQueens(n)
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  (queens(8) map show) mkString "\n"
}