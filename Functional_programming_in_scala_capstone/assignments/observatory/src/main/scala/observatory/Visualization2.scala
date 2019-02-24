package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature =
  {
    val x = point.x
    val y = point.y
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  def rollOverHeight(lat: Int): Int ={
    if(lat >= MaxLatitudeVisualization) MinLatitudeVisualization
    else lat + 1
  }

  def rollOverWidth(lon: Int): Int ={
    if(lon >= MaxLongitudeVisualization) MinLongitudeVisualization
    else lon + 1
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image =
  {
    val tilesCoordinates = (0 until TileHeight).flatMap(j => (0 until TileWidth).map(i => (i, j))).par
    val pixels = tilesCoordinates.map{
      case (i, j) => {
        val location = Interaction.tileLocation(
          Tile(TileWidth * tile.x + i, TileHeight * tile.y + j, tile.zoom + 8)
        )

        val latCeil = math.ceil(location.lat).toInt
        val latFloor = math.floor(location.lat).toInt
        val lonFloor = math.floor(location.lon).toInt
        val lonCeil = math.ceil(location.lon).toInt

        val upperLeftLat = if(latCeil == -90.0) 90 else latCeil
        val upperLeftLon = if(lonFloor == 180.0)  -180 else lonFloor

        val cellPoint = CellPoint(location.lon - lonFloor, latCeil - location.lat)
        val d00 = grid(GridLocation(upperLeftLat, upperLeftLon))
        val d01 = grid(GridLocation(upperLeftLat - 1, upperLeftLon))
        val d10 = grid(GridLocation(upperLeftLat, upperLeftLon + 1))
        val d11 = grid(GridLocation(upperLeftLat - 1, upperLeftLon + 1))
        val temperature = bilinearInterpolation(cellPoint, d00, d01, d10, d11)
        val color = Visualization.interpolateColor(colors, temperature)

        Pixel(RGBColor(color.red, color.green, color.blue, 127))
      }
    }.toArray[Pixel]
    Image(TileWidth, TileHeight, pixels)
  }
}
