package Lecture2_5

object test{
  val problem: Pouring = new Pouring(Vector(4, 7, 3))

  /*def until(paths: Stream[problem.Path], target: Int): List[problem.Path] = {
    val possiblePaths = paths filter(_.endState contains target)
    if( !possiblePaths.isEmpty ) possiblePaths.toList
    else {
      val newPaths = (paths flatMap (problem.moves map _.extend))
      if( newPaths.isEmpty ) throw new Error("No path for this target")
      else until(paths ++ newPaths, target)
    }
  }

  println(until(Stream(problem.initialPath), target = 2))*/

  println(problem.solutions(6).take(10).toList)
}
