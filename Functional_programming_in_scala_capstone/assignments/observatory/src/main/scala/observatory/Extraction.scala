package observatory

import java.io.InputStream
import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction {
  /**
    * @param temp temperature in Fahrenheit degree
    * @return     temperature in Celsius degree
    */
  def toCelsius(temp: Temperature): Temperature ={
    (temp - 32) * 5 / 9
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    // Stations file
    val stationsStream: InputStream = getClass.getResourceAsStream(stationsFile)
    //val stationsStream: InputStream = getClass.getResourceAsStream(s"/$stationsFile")
    val stationsLines = scala.io.Source.fromInputStream(stationsStream).getLines()
    /*
     * a line column
     * STN identifier, WBAN identifier, Latitude, Longitude
     */
    val validStationsMap = stationsLines.map(_.split(",", -1)).
      filter(words => (words(0).length > 0 || words(1).length > 0) &&
                       words(2).length > 0 &&
                       words(3).length > 0
      ).map(words => (words(0), words(1)) -> Location(words(2).toDouble, words(3).toDouble)).toMap

    // Temperatures file
    val temperaturesStream: InputStream = getClass.getResourceAsStream(temperaturesFile)
    //val temperaturesStream: InputStream = getClass.getResourceAsStream(s"/$temperaturesFile")
    val temperaturesLines = scala.io.Source.fromInputStream(temperaturesStream).getLines().toStream
    /*
     * a line column
     * STN identifier, WBAN identifier, Month, Day, Temperature
     */
    temperaturesLines.map(_.split(",", -1)).
      filter(words => (words(0).length > 0 || words(1).length > 0) &&
                       words(2).length > 0 &&
                       words(3).length > 0 &&
                       words(4).length > 0 &&
                       validStationsMap.contains((words(0), words(1)))
            ).
      map(words => (
                    LocalDate.of(year, words(2).toInt, words(3).toInt),
                    validStationsMap((words(0), words(1))),
                    toCelsius(words(4).toDouble)
                   )
      )
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.map{case (_, location, temperature) => (location, temperature)}.
      groupBy(_._1).mapValues(locationAndTemperatures => {
        val temperatures = locationAndTemperatures.map(_._2)
        temperatures.sum / temperatures.size
      }).seq
  }
}
