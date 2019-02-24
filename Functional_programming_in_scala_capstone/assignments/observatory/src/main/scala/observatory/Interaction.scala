package observatory

import com.sksamuel.scrimage.{Image}
import scala.collection.parallel.immutable.ParSeq

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    if(tile.zoom <= 0){
      Location(MaxLatitudeTile, MinLongitudeTile)
    } else {
      val maxTileCoordinatesValue: BigDecimal = (BigDecimal(2).pow(tile.zoom) - 1)
      /*
     * multiply by 2 to obtain a ratio between 0 and 2
     * then substract 1 to obtain a ration between -1 and 1
     */
      val yRatio = ((tile.y / maxTileCoordinatesValue) * -2 + 1).toDouble
      val latitude = MaxLatitudeTile * yRatio

      val xRatio = ((tile.x / maxTileCoordinatesValue) * 2 - 1).toDouble
      val longitude = MaxLongitudeTile * xRatio

      Location(latitude, longitude)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    parTile(temperatures.toIndexedSeq.par, colors, tile)
  }

  def parTile(temperatures: ParSeq[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val i_j = (0 until TileHeight).flatMap(j => (0 until TileWidth).map(i => (i, j))).par
    val locations = i_j.map{
      case (i, j) => tileLocation(
        Tile(TileWidth * tile.x + i, TileHeight * tile.y + j, tile.zoom + 8)
      )
    }

    Visualization.parVisualize(temperatures, colors, locations, TileWidth, TileHeight)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    val zoomLevels = 0 to 3
    val tiles: Iterable[Tile] = zoomLevels.flatMap(zoomLevel => {
        val tileBound = math.pow(2, zoomLevel).toInt
        val yCoordinates = 0 until tileBound
        val xCoordinates = 0 until tileBound
        val tiles = yCoordinates.flatMap(j => xCoordinates.map(i => Tile(i, j, zoomLevel)))
        tiles
      }
    )
    yearlyData.foreach{
      case (year, data) => tiles.foreach(tile => generateImage(year, tile, data))
    }
  }

}
