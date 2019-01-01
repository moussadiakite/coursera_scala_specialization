object pattern_matching {
  trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  case class Var(name: String) extends Expr

  object NoExpr extends Expr

  def eval(e: Expr): Int = e match{
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = e match{
    case Number(n) => n.toString
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) => (e1, e2) match{
      case (Sum(_, _), Sum(_, _)) => "(" + show(e1) + ") * (" + show(e2) + ")"
      case (Sum(_, _), _) => "(" + show(e1) + ") * " + show(e2)
      case (_, Sum(_, _)) => show(e1) + " * (" + show(e2) + ")"
      case (_, _) => show(e1) + " * " + show(e2)
    }
    case Var(name) => name
    case NoExpr => "Bambola"
  }

  val sum1 = show(Sum(Prod(Number(2), Var("x")), Var("y")))
  val sum2 = show(Prod(Sum(Number(2), Var("x")), Var("y")))
  val sum3 = show(Prod(Var("x"), Sum(Number(2), Var("y"))))
  val sum4 = show(Prod(Sum(Number(2), Var("x")), Sum(Number(2), Var("x"))))
}