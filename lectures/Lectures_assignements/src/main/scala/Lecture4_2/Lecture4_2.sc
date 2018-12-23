import java.util.NoSuchElementException

object exercise {

  trait List[+T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]

    def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
  }

  // Companion object
  object List{
    def apply[T](elem1: T, elem2:T): List[T] = new Cons(elem1, new Cons(elem2, Nil))
    def apply[T](elem: T): List[T] = new Cons(elem, Nil)
    def apply[T](): List[T] = Nil
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty: Boolean = false
  }

  object Nil extends List[Nothing] {
    def isEmpty: Boolean = true

    def head: Nothing = throw new NoSuchElementException

    def tail: Nothing = throw new NoSuchElementException
  }

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def incl(x: Int): NonEmpty = new NonEmpty(x, Empty, Empty)
    def contains(x: Int):Boolean = false
    def union(other: IntSet): IntSet = other
    override def toString: String = "."
  }

  class NonEmpty(value: Int, leftChild: IntSet, rightChild: IntSet) extends IntSet{
    def incl(x: Int): NonEmpty = {
      if(x == value) this
      else if (x < value) new NonEmpty(value, leftChild.incl(x), rightChild)
      else new NonEmpty(value, leftChild, rightChild.incl(x))
    }

    def contains(x: Int): Boolean = {
      if(x == value) true
      else if (x < value) leftChild.contains(x)
      else rightChild.contains(x)
    }

    def union(other: IntSet): IntSet =
      ((leftChild union rightChild) union other) incl value

    override def toString: String = "{" + leftChild + value + rightChild + "}"
  }

  val l = new Cons(new NonEmpty(5, Empty, Empty), Nil)
  l.prepend(Empty)
}