package Lecture3_1

object intSets {
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

  val t1 = new NonEmpty(3, Empty, Empty)
  val t3 = t1 incl 4
}
