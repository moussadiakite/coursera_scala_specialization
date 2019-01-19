import java.util.concurrent.ForkJoinTask

import common.common._

object exercise{
  trait Iterator[T] {
    def hasNext: Boolean
    def next(): T
    def foldLeft[S](z: S)(f: (S, T) => S): S = {
      var result = z
      if(hasNext) result = f(z, next())
      result
    }
  }

  val threshold = 2
  trait Task[T]
  trait Splitter[T] extends Iterator[T]{
    def split: Seq[Splitter[T]]
    def remaining: Int

    def fold(z: T)(f: (T, T) => T): T = {
      if (remaining < threshold) foldLeft(z)(f)
      else {
        val children: Seq[ForkJoinTask[T]] = for(child <- split) yield task(child.fold(z)(f))
        children.map(_.join()).foldLeft(z)(f)
      }
    }
  }
}