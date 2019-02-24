package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.apache.spark.rdd.RDD
//import sqlC.implicits._

trait VisualizationTest extends FunSuite with Checkers {
  val locationsAndTemperatures =
    Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
  val temperaturesFromFile: Iterable[(Location, Temperature)] =
    Extraction.locationYearlyAverageRecords(locationsAndTemperatures)
  val colors =
    List(
      (-60.0, Color(0, 0, 0)),
      (-50.0, Color(33, 0, 107)),
      (-27.0, Color(255, 0, 255)),
      (-15.0, Color(0, 0, 255)),
      (0.0, Color(0, 255, 255)),
      (12.0, Color(255, 255, 0)),
      (32.0, Color(255, 0, 0)),
      (60.0, Color(255, 255, 255))
    )

  def time[R](message: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"Elapsed time for $message: ${(t1 - t0) / 1e9}s")
    result
  }

  test("Great circle distance"){
    val sanFrancisco: Location = Location(37.6189, -122.375)
    val paris: Location = Location(49.01278, 2.5498)
    time("Great circle distance") {
      val distance = Visualization.greatCircleDistance(sanFrancisco, paris).toInt
      assert(distance === 8962)
    }
  }

  test("Interpolated color temperature exceeding max temperature"){
    val colors = List((-89.7604504165977,Color(255,0,0)), (15.221457727853732,Color(0,0,255)))
    time("Interpolated color temperature exceeding max temperature") {
      val predictedColor = Visualization.interpolateColor(colors, 20)
      assert(predictedColor === Color(0, 0, 255))
    }
  }

  test("Interpolated color simple interpolation 1"){
    val colors = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255)))
    time("Interpolated color simple interpolation 1") {
      val predictedColor = Visualization.interpolateColor(colors, -0.5)
      assert(predictedColor === Color(128, 0, 128))
    }
  }

  test("Interpolated color simple interpolation 2"){
    val colors = List((5.0,Color(255,0,0)), (30.0,Color(0,0,255)))
    time("Interpolated color simple interpolation 2") {
      val predictedColor = Visualization.interpolateColor(colors, -0.5)
      assert(predictedColor === Color(128, 0, 128))
    }
  }

  test("Test predictTemperature from file with Stream") {
    time("Test predictTemperature from file with Stream") {
      val temp = Visualization.parPredictTemperature(temperaturesFromFile.toStream.par, Location(50, 50))
    }
  }

  test("Test predictTemperature from file with Vector") {
    time("Test predictTemperature from file with Vector") {
      val temp = Visualization.parPredictTemperature(temperaturesFromFile.toVector.par, Location(50, 50))
    }
  }

  test("Test predictTemperature from file with IndexedSeq") {
    time("Test predictTemperature from file with IndexedSeq") {
      val temp = Visualization.parPredictTemperature(temperaturesFromFile.toIndexedSeq.par, Location(50, 50))
    }
  }

  test("Test visualize sample temperatures") {
    val temperatures = List((Location(45.0, -90.0), 9.899607001302812), (Location(-45.0, 0.0), 23.855278181670343))
    time("Test visualize") {
      val image = Visualization.visualize(temperatures, colors)
      assert(image.count == 64800)
    }
  }

  test("Test visualize temperatures from file") {
    time("Test visualize temperatures from file") {
      val image = Visualization.visualize(temperaturesFromFile, colors)
      assert(image.count == 64800)
    }
  }

  /*test("Test visualize rdd") {
    val locationsAndTemperatures =
      SparkExtraction.locateTemperaturesRDD(1975, "stations.csv", "1975.csv")
    val locationYearlyAverageRecords =
      SparkExtraction.locationYearlyAverageRecordsDataFrame(locationsAndTemperatures)
    val locationYearlyAverageRecordsRDD: RDD[(Location, Temperature)] = locationYearlyAverageRecords.rdd.
      map(row => {
        try {
          (Location(row.getAs[Double](0), row.getAs[Double](1)), row.getAs[Double](2))
        } catch {
          case unknown: java.lang.ClassCastException => {
            val latitude = ("" + row.getAs[String](0)).toDouble
            val longitude = ("" + row.getAs[String](1)).toDouble
            val temperature = ("" + row.getAs[String](2)).toDouble
            (Location(latitude, longitude), temperature)
          }
        }
      })
    time("Test visualize temperatures RDD from file") {
      val image = Visualization.visualizeRDD(locationYearlyAverageRecordsRDD, colors)
      sqlC.sparkContext.stop()
      sqlC.stop()
      assert(image.count == 64800)
    }
  }*/
}
