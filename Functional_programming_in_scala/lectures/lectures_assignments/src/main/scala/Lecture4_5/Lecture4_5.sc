object classification_accessors {
  trait Expr {
    def isNumber: Boolean

    def isSum: Boolean

    def numValue: Int

    def rightOp: Expr

    def leftOp: Expr
  }

  class Number(n: Int) extends Expr {
    def isNumber: Boolean = true

    def isSum: Boolean = false

    def numValue: Int = n

    def rightOp: Nothing = throw new Error("Number.rightOp")

    def leftOp: Nothing = throw new Error("Number.rightOp")
  }

  class Sum(e1: Expr, e2: Expr) extends Expr {
    def isNumber: Boolean = false

    def isSum: Boolean = true

    def numValue: Int = throw new Error("Sum.numValue")

    def rightOp: Expr = e1

    def leftOp: Expr = e2
  }

  def eval(e: Expr): Int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unknown expression " + e)
  }
}

object decomposition {
  trait Expr {
    def eval: Int

    def rightOp: Expr

    def leftOp: Expr
  }

  case class Number(n: Int) extends Expr {
    def eval: Int = n

    def rightOp: Nothing = throw new Error("Number.rightOp")

    def leftOp: Nothing = throw new Error("Number.leftOp")
  }

  case class Sum(e1: Expr, e2: Expr) extends Expr {
    def eval: Int = e1.eval + e2.eval

    def rightOp: Expr = e1

    def leftOp: Expr = e2
  }

  case class Product(e1: Expr, e2: Expr) extends Expr {
    def eval: Int = e1.eval * e2.eval

    def rightOp: Expr = e1

    def leftOp: Expr = e2
  }

  case class Var(s: String) extends Expr {
    def name: String = s

    def eval: Int = throw new Error("Var.eval")

    def rightOp: Expr = throw new Error("Var.rightOp")

    def leftOp: Expr = throw new Error("Var.leftOp")
  }
}