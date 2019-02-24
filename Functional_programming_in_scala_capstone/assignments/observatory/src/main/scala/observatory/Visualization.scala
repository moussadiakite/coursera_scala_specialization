package observatory

import akka.japi.Option.Some
import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import org.apache.spark.sql.DataFrame
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.functions.{callUDF, col, sum}

import scala.collection.parallel.ParIterable
import scala.language.postfixOps
import org.apache.spark.storage.StorageLevel

import scala.collection.parallel.immutable.ParSeq


/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  //var count = 0

  //sqlC.udf.register("inverseDistance", (distance: Double) => 1 / (distance * distance))

  /**
    * @param location1
    * @param location2
    * @return The great circle distance between location1 and location2
    */
  def greatCircleDistance(location1: Location, location2: Location): Distance = {
    if(location1.lat == location2.lat && location1.lon == location2.lon) 0.0
    else {
      val longitudeDelta = math.abs(location1.lon - location2.lon)
      val cosCentralAngle = math.sin(math.toRadians(location1.lat)) * math.sin(math.toRadians(location2.lat)) +
        math.cos(math.toRadians(location1.lat)) * math.cos(math.toRadians(location2.lat)) * math.cos(math.toRadians(longitudeDelta))
      if(cosCentralAngle < -1.0) {
        EarthRadius * math.acos(-1.0)
      } else if(cosCentralAngle > 1.0) {
        0.0 // EarthRadius * math.acos(1.0)
      } else {
        EarthRadius * math.acos(cosCentralAngle)
      }
    }
  }

  //def memoizedGreatCircleDistance = memoizeFnc[(Location, Location), Distance](greatCircleDistance _ tupled)

  def flatGreatCircleDistance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Distance = {
    if(lat1 == lat2 && lon1 == lon2) 0.0
    else {
      val longitudeDelta = math.abs(lon1 - lon2)
      val cosCentralAngle = math.sin(math.toRadians(lat1)) * math.sin(math.toRadians(lat2)) +
        math.cos(math.toRadians(lat1)) * math.cos(math.toRadians(lat2)) * math.cos(math.toRadians(longitudeDelta))
      if(cosCentralAngle < -1.0) {
        EarthRadius * math.acos(-1.0)
      } else if(cosCentralAngle > 1.0) {
        0.0 // EarthRadius * math.acos(1.0)
      } else {
        EarthRadius * math.acos(cosCentralAngle)
      }
    }
  }

  //def memoizedFlatGreatCircleDistance = memoizeFnc[(Double, Double, Double, Double), Distance](flatGreatCircleDistance _ tupled)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    parPredictTemperature(temperatures.toIndexedSeq.par, location)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def parPredictTemperature(temperatures: ParSeq[(Location, Temperature)], location: Location): Temperature = {
    /*val (tempWeights: ParSeq[Temperature], weights: ParSeq[Distance]) = temperatures.map {
      case (otherLocation, temperature) => {
        val distance = greatCircleDistance(otherLocation, location)
        if(distance < 1.0) return temperature
        val weight = 1.0 / (distance * distance)
        (temperature * weight, weight)
      }
    }.unzip
    tempWeights.sum / weights.sum*/

    val (tempWeights, weights) = temperatures.aggregate((0.0, 0.0))(
      {
        case ((tempWeights, weights), (otherLocation, temperature)) => {
          val distance = greatCircleDistance(otherLocation, location)
          if (distance < 1.0) return temperature
          val weight = 1.0 / (distance * distance)
          (tempWeights + temperature * weight, weights + weight)
        }
      },
      {
        case ((tempWeights1, weights1), (tempWeights2, weights2)) =>
          (tempWeights1 + tempWeights2, weights1 + weights2)
      }
    )
    tempWeights / weights
  }

  /**
    * @param temperatures Known temperatures: dataframe containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperatureRDD(temperatures: RDD[(Location, Temperature)], location: Location): Temperature = {
    val (tempWeights, weights) = temperatures.map {
      case (otherLocation, temperature) => {
        val distance = greatCircleDistance(otherLocation, location)
        if(distance < 1.0) return temperature
        val weight = 1.0 / (distance * distance)
        (temperature * weight, weight)
      }
    }.collect().par.unzip
    tempWeights.sum / weights.sum
  }

  /**
    * @param temperatures Known temperatures: dataframe containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperatureDataframe(temperatures: DataFrame, location: Location): Temperature = {
    //count = count + 1
    //println(count)
    sqlC.udf.register(
      "greatCircleDistance",
      (latitude2: Double, longitude2: Double) =>
        flatGreatCircleDistance(location.lat, location.lon, latitude2, longitude2))
    val distances: DataFrame = temperatures.withColumn(
      "Distance",
      callUDF(
        "greatCircleDistance", col("Latitude"), col("Longitude")
      )
    )
    //distances.persist(StorageLevel.MEMORY_ONLY)
    val min = distances.orderBy(col("Distance")).
              head
    if(min.getAs[Double](3) < 1.0){
      min.getAs[Double](2)
    } else {
      val tempWeights: DataFrame = distances.withColumn(
        "InverseDistance",
        callUDF("inverseDistance", col("Distance"))
      )
      tempWeights.persist(StorageLevel.MEMORY_ONLY)
      val weightsSum = tempWeights.
        agg(sum(col("inverseDistance")) as "Wheights Sum").
        head.getAs[Double](0)
      val ponderations = tempWeights.withColumn("Ponderations", col("Temperature") * col("InverseDistance")).
        agg(sum(col("Ponderations"))).
        head.getAs[Double](0)
      ponderations / weightsSum
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    /*
     * interpolation function: I(x)= sum (k from 1 to N) (y_k * product(i from 1 to N, i ≠ k) ((x - x_i) / (x_k - x_i)))
     */
    if(points.isEmpty) DefaultColor
    val pointsVector: Vector[(Temperature, Color)] = points.toVector.sortWith(_._1 > _._1)
    val min = pointsVector.last
    val max = pointsVector.head
    if(value <= min._1) {
      min._2
    } else if(value >= max._1) {
      max._2
    } else {
      val lowerEndPointIndex = pointsVector.indexWhere(_._1 <= value)
      val lowerEndPoint = pointsVector(lowerEndPointIndex)
      val upperEndPoint = pointsVector(lowerEndPointIndex - 1)

      val x0 = lowerEndPoint._1
      val y0 = lowerEndPoint._2

      val x1 = upperEndPoint._1
      val y1 = upperEndPoint._2

      val factor = (value - x0) / (x1 - x0)
      y0 * (1 - factor) + y1 * factor
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    parVisualize(temperatures.toIndexedSeq.par, colors, defaultLocations.par, VisualizationImageWidth, VisualizationImageHeight)
  }

  def parVisualize(temperatures: ParSeq[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    parVisualize(temperatures, colors, defaultLocations.par, VisualizationImageWidth, VisualizationImageHeight)
  }

  def visualize(temperatures: Iterable[(Location, Temperature)],
                   colors: Iterable[(Temperature, Color)],
                   locations: ParIterable[Location],
                   imageWidth: Int,
                   imageHeight: Int): Image = {
    parVisualize(temperatures.toIndexedSeq.par, colors, locations, imageWidth, imageHeight)
  }

  def parVisualize(temperatures: ParSeq[(Location, Temperature)],
                   colors: Iterable[(Temperature, Color)],
                   locations: ParIterable[Location],
                   imageWidth: Int,
                   imageHeight: Int): Image = {
    val pixels: Array[Pixel] = locations.map(location => {
      val temperature = parPredictTemperature(temperatures, location)
      val color = interpolateColor(colors, temperature)
      Pixel(RGBColor(color.red, color.green, color.blue, 127))
    }
    ).toArray[Pixel]
    Image(imageWidth, imageHeight, pixels)
  }

  def visualizeRDD(temperatures: RDD[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    visualizeRDD(temperatures, colors, defaultLocations.par, VisualizationImageWidth, VisualizationImageHeight)
  }

  def visualizeRDD(temperatures: RDD[(Location, Temperature)],
                   colors: Iterable[(Temperature, Color)],
                   locations: ParIterable[Location],
                   imageWidth: Int,
                   imageHeight: Int): Image = {
    temperatures.persist()
    val pixels: Array[Pixel] = locations.map(location => {
      val temperature = predictTemperatureRDD(temperatures, location)
      val color = interpolateColor(colors, temperature)
      Pixel(RGBColor(color.red, color.green, color.blue, 127))
    }
    ).toArray[Pixel]
    Image(imageWidth, imageHeight, pixels)
  }

  /**
    * @param temperatures Known temperatures containing a location and the temperature at this location
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualizeDataFrame(temperatures: DataFrame, colors: Iterable[(Temperature, Color)]): Image = {
    val allLocationsTemperatures = defaultLocations.par.map(location => predictTemperatureDataframe(temperatures, location))
    val pixels: Array[Pixel] = allLocationsTemperatures.map(
      temperature => {
        val color = interpolateColor(colors, temperature)
        Pixel(RGBColor(color.red, color.green, color.blue, 127))
      }
    ).toArray[Pixel]
    Image(VisualizationImageWidth, VisualizationImageHeight, pixels)
  }
}
