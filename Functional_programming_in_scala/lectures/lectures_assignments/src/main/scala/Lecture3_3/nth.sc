package Lecture3_3

import Lecture3_3._

object exercise {
  def nth[T](n: Int, l: List[T]): T = {
    if(l.isEmpty || n < 0) throw IndexOutOfBoundsException
    else if(n == 0) l.head
    else nth(n - 1, l.tail)
  }
}