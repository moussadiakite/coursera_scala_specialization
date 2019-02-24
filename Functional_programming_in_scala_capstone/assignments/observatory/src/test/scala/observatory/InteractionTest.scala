package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalameter._

trait InteractionTest extends FunSuite with Checkers {
  test("Test tile 1") {
    val temperatures = List((Location(45.0, -90.0), 20.0), (Location(45.0, 90.0), 0.0), (Location(0.0, 0.0), 10.0), (Location(-45.0, -90.0), 0.0), (Location(-45.0, 90.0), 20.0))
    val scale = List((0.0, Color(255, 0, 0)), (10.0, Color(0, 255, 0)), (20.0, Color(0, 0, 255)))

    val time = config(
      Key.exec.minWarmupRuns -> 1,
      Key.exec.maxWarmupRuns -> 10
    ) withWarmer(new Warmer.Default) measure {
      val image = Interaction.tile(temperatures, scale, Tile(0, 0, 0))
      //assert(image.count == 65536)
    }
    println(s"Test tile 1 time $time ms")
  }

  test("Test tile 2") {
    val temperatures = List((Location(45.0, -90.0), 5.0), (Location(-45.0, 0.0), 30.0))
    val scale = List((5.0, Color(255, 0, 0)), (30.0, Color(0, 0, 255)))

    val time = config(
      Key.exec.minWarmupRuns -> 1,
      Key.exec.maxWarmupRuns -> 10
    ) withWarmer(new Warmer.Default) measure {
      val image = Interaction.tile(temperatures, scale, Tile(0, 0, 0))
      //assert(image.count == 65536)
    }
    println(s"Test tile 2 time $time ms")
  }
}
