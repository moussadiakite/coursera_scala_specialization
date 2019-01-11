package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.math.min
import scala.math.max

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    oneOf(
      const(empty),
      for{
        x <- arbitrary[A]
        h <- genHeap
      } yield insert(x, h)
    )
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("insert empty then delete") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  def getElementsReverse(h: H, acc: List[A]): List[A] =
    if(isEmpty(h)) acc
    else {
      val min = findMin(h)
      getElementsReverse(deleteMin(h), min::acc)
    }

  def isSorted(l: List[A]): Boolean = l match {
    case Nil => true
    case _::Nil => true
    case x1::x2::xs => ord.lteq(x1, x2) && isSorted(x2::xs)
  }

  property("sorted list") = forAll { (h: H) =>
    isSorted(getElementsReverse(h, List()).reverse)
  }

  property("minimum of melding") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    if(isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin(m) == findMin(h2)
    else if (isEmpty(h2)) findMin(m) == findMin(h1)
    else findMin(m) == min(findMin(h1), findMin(h2))
  }

  property("min3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    findMin(h) == min(min(a, b), c)
  }

  property("min4") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    findMin(deleteMin(deleteMin(h))) == max(max(a, b), c)
  }
}
