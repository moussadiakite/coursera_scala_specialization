package scalashop

import org.scalameter._
import common._

object RectangularBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      RectangularBoxBlur.blur(src, dst, (0, 0), (width, height), radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      RectangularBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object RectangularBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    *  starting with `from` and ending with `end` (non-inclusive).
    *
    *  Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: (Int, Int), end: (Int, Int), radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    val fromX = from._1
    val fromY = from._2
    val endX = end._1
    val endY = end._2

    for(j <- fromY until endY){
      for(i <- fromX until endX){
        dst(i, j) = boxBlurKernel(src, i, j, radius)
      }
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    *  Parallelization is done by stripping the source image `src` into
    *  `numTasks` separate strips, where each strip is composed of some number of
    *  rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    /* we want to split the image into small rectangular images proportional to
     * the initial image. If we call "k" this proportionality coefficient then
     * it means we want small rectangle of with dimensions (width / k), (height / k)
     * and we want numtasks rectangle like this. The surface of the initial
     * image is therefore equals to the surface of a small rectangle mutliplied
     * by numtasks. We then have the relation:
     * width * height = numtasks * (width / k) * (height / k)
     * It's then easy to deduce that k = sqrt(numtasks)
     */
    val k = Math.round(Math.sqrt(numTasks)).toInt
    val stepX = Math.round(src.width.toDouble / k).toInt
    val stepY = Math.round(src.height.toDouble / k).toInt
    if(stepX == 0 && stepY == 0){
      blur(src, dst, (0, 0), (src.width, src.height), radius)
    } else {
      val rangeX = if(stepX == 0) List(0) else (0 until src.width by stepX).toList
      val rangeY = if(stepY == 0) List(0) else (0 until src.height by stepY).toList
      val splitting_points_start = rangeY.map(startY => rangeX.map(startX => (startX, startY)))
      val splitting_points = splitting_points_start.flatten zip
        (
          splitting_points_start.tail.map(list => list.tail ++ List((src.width, list.head._2))).flatten ++
            (stepY until src.width by stepY).map((_, src.height)) ++ List((src.width, src.height))
          )
      //println(s"k = $k, stepX = $stepX, stepY = $stepY, points = $splitting_points")
      val tasks = splitting_points.map {
        case (start, end) => task(blur(src, dst, start, end, radius))
      }
      tasks.map(_.join())
    }
  }
}
