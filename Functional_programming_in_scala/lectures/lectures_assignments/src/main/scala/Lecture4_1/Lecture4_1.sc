object exercise{
  abstract class Boolean{
    def ifThenElse[T](t: => T, e: => T): T // if(cond) t else e <=> cond.ifThenElse(t, e)

    def &&(other: Boolean): Boolean = ifThenElse[Boolean](other, False)

    def ||(other: Boolean): Boolean = ifThenElse(True, other)

    def unary_!(): Boolean = ifThenElse(False, True)

    def ==(other: Boolean): Boolean = ifThenElse(other, !other)

    def !=(other: Boolean): Boolean = !(this == other)

    def <(other: Boolean):Boolean = ifThenElse(False, other)
  }

  object True extends Boolean{
    def ifThenElse[T](t: => T, e: => T) = t
  }

  object False extends Boolean{
    def ifThenElse[T](t: => T, e: => T) = e
  }

  abstract class Nat{
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def +(that: Nat): Nat
    def -(that: Nat): Nat

    override def toString: String
  }

  object Zero extends Nat{
    override def isZero: Boolean = True

    override def predecessor: Nat = throw new Error("Zero.predecessor")

    override def successor: Nat = new Succ(Zero)

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat = that.isZero.ifThenElse(this, throw new Error("Negative number"))

    override def toString: String = "Zero"
  }

  class Succ(n: Nat) extends Nat{
    override def isZero: Boolean = False

    override def predecessor:Nat = n

    override def successor: Nat = new Succ(this)

    override def +(that: Nat): Nat = new Succ(n + that)

    override def -(that: Nat): Nat = that.isZero.ifThenElse(this, this.predecessor - that.predecessor)

    override def toString: String = "Succ( " + n + " )"
  }

  val n = new Succ(Zero)
  val m = n.successor
  val l = m.successor
  val k = l + m
  val j = k - n
}