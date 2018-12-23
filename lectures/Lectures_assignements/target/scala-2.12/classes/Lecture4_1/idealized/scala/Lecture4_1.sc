package Lecture4_1.idealized.scala

abstract class Boolean{
  def ifThenElse[T](t: => T, e: => T): T // if(cond) t else e <=> cond.ifThenElse(t, e)

  def &&(other: Boolean): Boolean = ifThenElse[Boolean](other, false)

  def ||(other: Boolean): Boolean = ifThenElse(true, other)

  def unary_!(): Boolean = ifThenElse(false, true)

  def ==(other: Boolean): Boolean = ifThenElse(other, !other)

  def !=(other: Boolean): Boolean = !(this == other)
}

  object true extends Boolean{
    ifThenElse[T](t: => T, e: => T) = t
  }

  object false extends Boolean{
    ifThenElse[T](t: => T, e: => T) = e
  }

  object Main extends App {
    println("HelloWord")
  }