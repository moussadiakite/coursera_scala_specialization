object polynomials {
  case class Poly(val terms: Map[Int, Double]) {
    val total_terms = terms withDefaultValue 0.0

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    //def +(other: Poly): Poly = Poly(total_terms ++ (other.terms map adjust))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coef) = term
      terms + (exp -> (coef + total_terms(exp)))
    }

    def +(other: Poly): Poly = new Poly((other.terms foldLeft total_terms)(addTerm))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coef) = term
      exp -> (coef + total_terms(exp))
    }

    override def toString: String = (for {
      (exp, coef) <- terms.toList.sorted.reverse
    } yield (coef + "x^" + exp)).mkString(" + ")
  }

  // Tests
  val map = Map(1 -> "Mali", 2 -> "Niger", 3 -> "Sénégal")

  val p1 = Poly(Map(0 -> 7, 1 -> 4, 2 -> 3.5, 4 -> 5.2))
  val p2 = Poly(Map(1 -> 5, 3 -> 2.3, 4 -> 1, 5 -> 6))
  p1 + p2
}