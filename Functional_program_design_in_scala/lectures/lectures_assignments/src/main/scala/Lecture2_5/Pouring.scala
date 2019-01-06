package Lecture2_5

import Lecture2_5.test.problem

class Pouring(capacity: Vector[Int]){
  // States

  type State = Vector[Int]

  val initialState = capacity map (_ => 0)

  // Moves

  trait Move{
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state.updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amountToPour = state(from) min (capacity(to) - state(to))
      state.updated(from, state(from) - amountToPour).updated(to, state(to) + amountToPour)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))

  // Paths

  class Path(history: List[Move], val endState: State){
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString(): String = (history.reverse mkString " ") + " --> " + endState

    /* private def trackState(xs: List[Move]): State = xs match {
       case Nil => initialState
       case move::ys => move change trackState(ys)
    } */
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    val more = paths flatMap (moves map _.extend) filter (path => ! (explored contains path.endState))
    paths #:: from(more, explored ++ (more map (_.endState)))
  }

  val pathSets: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
