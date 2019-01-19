package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    val angles = input.tail.zipWithIndex.map{case (height, i) => height / (i + 1)}
    val cumulativeAngles = angles.scanLeft(0.toFloat)((x, y) => (x max y).toInt)
    cumulativeAngles.copyToArray(output)
  }

  sealed abstract class Tree {
    def maxPrevious: Float
    def toString: String
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)

    override def toString: String = "Node ( " + left + ", " + right + " )"
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree {
    override def toString: String = "Leaf ( from = " + from + ", until = " + until + ", maxPrevious = " + maxPrevious
  }

  def validIndexes(from: Int, until: Int, arrayLength: Int): Boolean = {
    from >= 0 && from < until && until <= arrayLength
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    if(!validIndexes(from, until, input.length)) Int.MinValue
    else {
      val maxAngle = if(from > 0) input(from) / from else 0.toFloat
      maxAngle max upsweepSequential(input, from + 1, until)
    }
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    val interval = end - from
    if(interval <= threshold){
      Leaf(from, end, upsweepSequential(input, from, end))
    } else {
      val mid = (from + end) / 2
      val (leftTree, rightTree) = parallel(
        upsweep(input, from, mid, threshold),
        upsweep(input, mid, end, threshold)
      )
      Node(leftTree, rightTree)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    if(validIndexes(from, until, input.length)){
      output(from) = if(from > 0) startingAngle max (input(from) / from) else startingAngle
      downsweepSequential(input, output, output(from), from + 1, until)
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
    case Leaf(from, until, _) => downsweepSequential(input, output, startingAngle, from, until)
    case Node(left, right) => {
      parallel(
        downsweep(input, output, startingAngle, left),
        downsweep(input, output, startingAngle max left.maxPrevious, right)
      )
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val reductionTree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 1.toFloat, reductionTree)
  }
}
