package observatory

//import org.apache.spark.rdd.RDD
//import org.apache.spark.sql.{DataFrame, Row}
//import org.apache.spark.storage.StorageLevel


object Main extends App {
  def time[R](message: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"Elapsed time for $message: ${(t1 - t0) / 1e9}s")
    result
  }

  def extractData(year: Year): Iterable[(Location, Temperature)] = {
    val locationsAndTemperatures = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
    val locationYearlyAverageRecords: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecords(locationsAndTemperatures)
    locationYearlyAverageRecords
  }

  val layers = Interaction2.availableLayers
  val temperaturesColors = layers.find(_.layerName == LayerName.Temperatures).get.colorScale
  val deviationsColors = layers.find(_.layerName == LayerName.Deviations).get.colorScale

  //temperatures
  var count = 0

  def generateImageTemperatures(year: Year, tile: Tile, yearlyData: Iterable[(Location, Temperature)]): Unit = {
    count = count + 1
    println(count)
    val image = Interaction.tile(yearlyData, temperaturesColors, tile)
    image.output(new java.io.File(s"target/temperatures/$year/${tile.zoom}/${tile.x}-${tile.y}.png"))
    ()
  }

  val years = 1975 to 1975
  val yearlyData: Iterable[(Year, Iterable[(Location, Temperature)])] = years.toStream.map(year => {
    (year, extractData(year))
  })
  Interaction.generateTiles[Iterable[(Location, Temperature)]](yearlyData, generateImageTemperatures)

  // Deviations

  val normalYears = 1975 to 1989
  val temperaturess: Stream[Iterable[(Location, Temperature)]] = normalYears.toStream.map(extractData)
  val normals: GridLocation => Temperature = Manipulation.average(temperaturess)

  val deviationYears = 1990 to 1990
  val yearlyDataDeviations = deviationYears.toStream.map(
    year => {
      (year, Manipulation.deviation(extractData(year), normals))
    }
  )

  def generateImageDeviations(year: Year, tile: Tile, yearlyData: GridLocation => Temperature): Unit = {
    count = count + 1
    println(count)
    val image = Visualization2.visualizeGrid(yearlyData, deviationsColors, tile)
    image.output(new java.io.File(s"target/deviations/$year/${tile.zoom}/${tile.x}-${tile.y}.png"))
    ()
  }

  Interaction.generateTiles[GridLocation => Temperature](yearlyDataDeviations, generateImageDeviations)

  /*time("rdd") {
    val locationsAndTemperatures =
      Extraction.locateTemperaturesRDD(1975, "stations.csv", "1975.csv")
    val locationYearlyAverageRecords =
      Extraction.locationYearlyAverageRecordsDataFrame(locationsAndTemperatures)
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
    locationYearlyAverageRecordsRDD.persist(StorageLevel.MEMORY_ONLY)
    Visualization.visualizeRDD(locationYearlyAverageRecordsRDD, colors)
    //Interaction.tile(locationYearlyAverageRecords, colors, Tile(0, 0, 0))
  }

  time("dataframe") {
    val locationsAndTemperatures: DataFrame =
      Extraction.locateTemperaturesDataFrame(1975, "stations.csv", "1975.csv")
    val locationYearlyAverageRecords: DataFrame =
      Extraction.locationYearlyAverageRecordsDataFrame(locationsAndTemperatures)
    locationYearlyAverageRecords.persist(StorageLevel.MEMORY_ONLY)
    //Visualization.visualizeDataFrame(locationYearlyAverageRecords, colors)
    //Interaction.tile(locationYearlyAverageRecords, colors, Tile(0, 0, 0))
  }
  sqlC.sparkContext.stop()
  sqlC.stop()*/
}
