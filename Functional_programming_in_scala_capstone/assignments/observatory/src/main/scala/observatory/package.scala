import org.apache.spark.sql.SparkSession
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.{immutable}

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type Distance = Double // Distance in km

  // Visualisation
  val ColorScale = 256
  val DefaultColor = Color(0, 0, 0)
  val EarthRadius: Double = 6371 // in km
  val MinLatitudeVisualization: Int = -89
  val MaxLatitudeVisualization: Int = 90
  val MinLongitudeVisualization: Int = -180
  val MaxLongitudeVisualization: Int = 179
  val defaultCoordinates: immutable.IndexedSeq[(Int, Int)] = (MaxLatitudeVisualization to MinLatitudeVisualization by -1).
    flatMap(latitude => (MinLongitudeVisualization to MaxLongitudeVisualization).
      map(longitude => (latitude, longitude)))
  val defaultLocations: immutable.IndexedSeq[Location] = (MaxLatitudeVisualization to MinLatitudeVisualization by -1).
    flatMap(latitude => (MinLongitudeVisualization to MaxLongitudeVisualization).
      map(longitude => Location(latitude, longitude)))
  val VisualizationImageHeight: Int = 180
  val VisualizationImageWidth: Int = 360
  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Observatory")
  //@transient lazy val sc: SparkContext = new SparkContext(conf)
  @transient lazy val sqlC: SparkSession = SparkSession.builder().master("local").appName("Observatory").getOrCreate()

  // Interaction
  val MinLatitudeTile: Double = -85.0511
  val MaxLatitudeTile: Double = 85.0511
  val MinLongitudeTile: Double = -180
  val MaxLongitudeTile: Double = 180
  val TileWidth: Int = 256
  val TileHeight: Int = 256
  val power = 8

  // Manipulation
  val GridHeight: Int = 180
  val GridWidth: Int = 360

  /*def memoizeFnc[K, V](f: K => V): mutable.HashMap[K, V] = new collection.mutable.HashMap[K, V]() {
    override def apply(key: K) = getOrElseUpdate(key, f(key))
  }*/
}
