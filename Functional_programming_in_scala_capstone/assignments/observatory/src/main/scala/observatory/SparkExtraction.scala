/*
package observatory

import observatory.sqlC.implicits._
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions.{avg, callUDF, col, lit}
import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */
object SparkExtraction {
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
  def locateTemperaturesRDD(year: Year, stationsFile: String, temperaturesFile: String): DataFrame =
  {
    // Stations file
    val stationsLines: RDD[String] = sqlC.sparkContext.textFile(s"./src/main/resources/$stationsFile")
    val validStations: DataFrame = stationsLines.map(_.split(",", -1)).
      filter(words => (words(0).length > 0 || words(1).length > 0) &&
        words(2).length > 0 &&
        words(3).length > 0
      ).map{case Array(stn, wban, latitude, longitude) => Station(stn, wban, longitude, latitude)}.
      toDF

    // Temperatures file
    val temperaturesLines: RDD[String] = sqlC.sparkContext.textFile(s"./src/main/resources/$temperaturesFile")
    /*
     * a line column
     * STN identifier, WBAN identifier, Month, Day, Temperature
     */
    val validTemperatures: DataFrame = temperaturesLines.map(_.split(",", -1)).
      filter(words => (words(0).length > 0 || words(1).length > 0) &&
        words(2).length > 0 &&
        words(3).length > 0 &&
        words(4).length > 0
      ).map{case Array(stn, wban, month, day, temperature) => TemperatureRecord(stn, wban, month, day, temperature)}.
      toDF

    val validData = validTemperatures.join(validStations, validTemperatures("STN") <=> validStations("STN") &&
      validTemperatures("WBAN") <=> validStations("WBAN"))

    validData.sparkSession.udf.register("toCelsius", toCelsius _)

    validData.withColumn("Year", lit(year)).withColumn(
      "Temperature",
      callUDF("toCelsius", col("temperature"))
    )
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A dataframe with columns (Year, Month, Day, Latitude, Longitude, Temperature)
    */
  def locateTemperaturesDataFrame(year: Year, stationsFile: String, temperaturesFile: String): DataFrame = {
    /*
     * Stations file with columns containsSTN identifier, WBAN identifier, Latitude, Longitude
     */
    val stationsSchema = StructType(
      Seq(
        StructField("STN", StringType, true),
        StructField("WBAN", StringType, true),
        StructField("Latitude", DoubleType, true),
        StructField("Longitude", DoubleType, true)
      )
    )
    val stations: DataFrame = sqlC.read.option("sep", ",").
      schema(stationsSchema).csv(s"./src/main/resources/$stationsFile")

    val validStations: DataFrame = stations.na.drop("any", Array("Latitude", "Longitude")).
      na.drop("all", Array("STN", "WBAN"))

    /* Temperatures file with columns STN identifier, WBAN identifier, Month, Day, Temperature
     */
    val temperaturesSchema = StructType(
      Seq(
        StructField("STN", StringType, true),
        StructField("WBAN", StringType, true),
        StructField("Month", IntegerType, true),
        StructField("Day", IntegerType, true),
        StructField("Temperature", DoubleType, true)
      )
    )
    val temperatures: DataFrame = sqlC.read.option("sep", ",").
      schema(temperaturesSchema).csv(s"./src/main/resources/$temperaturesFile")

    val validTemperatures = temperatures.na.drop("any", Array("Month", "Day", "Temperature")).
      na.drop("all", Array("STN", "WBAN"))

    val validData = validTemperatures.join(validStations, validTemperatures("STN") <=> validStations("STN") &&
                                                          validTemperatures("WBAN") <=> validStations("WBAN"))

    validData.sparkSession.udf.register("toCelsius", toCelsius _)

    validData.withColumn("Year", lit(year)).withColumn(
      "Temperature",
      callUDF("toCelsius", col("temperature"))
    )
  }

  /**
    * @param records A dataframe with columns (Year, Month, Day, Latitude, Longitude, Temperature)
    * @return A dataframe containing for each location, the average temperature over the year.
    * The year column in record contains the same value
    */
  def locationYearlyAverageRecordsDataFrame(records: DataFrame): DataFrame = {
    records.groupBy("Latitude", "Longitude").agg(avg("Temperature") as "Temperature").
      select("Latitude", "Longitude", "Temperature")
  }

}
*/
