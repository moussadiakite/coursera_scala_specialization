package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */

  def position(latitude: Int, longitude: Int): Int = {
    val x = 180 + longitude
    val y = 90 - latitude
    y * GridWidth + x
  }

  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
      val cache = new Array[Temperature](GridWidth * GridHeight)
      defaultCoordinates.par.foreach{
        case (lat, lon) =>
            cache(position(lat, lon)) = Visualization.parPredictTemperature(temperatures.toIndexedSeq.par, Location(lat, lon))
        }
    (gridLocation: GridLocation) => cache(position(gridLocation.lat, gridLocation.lon))
  }

  def combineGrid(grid1: GridLocation => Temperature, grid2: GridLocation => Temperature): GridLocation => Temperature = {
    val resultGrid = defaultCoordinates.par.map {
      case (lat, lon) => {
        val gridLocation = GridLocation(lat, lon)
        (grid1(gridLocation) + grid2(gridLocation)) / 2
      }
    }
    (gridLocation: GridLocation) => resultGrid(position(gridLocation.lat, gridLocation.lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    if(temperaturess.nonEmpty) {
      val firstGrid = makeGrid(temperaturess.head)
      temperaturess.tail.par.aggregate(firstGrid)(
        {
          case (grid, temperatures) => {
            val otherGrid = makeGrid(temperatures)
            combineGrid(grid, otherGrid)
          }
        },
        {
          case (grid1, grid2) => {
            combineGrid(grid1, grid2)
          }
        }
      )
    } else {
      (gridLocation: GridLocation) => 0.0
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature =
  {
    val grid = makeGrid(temperatures)
    (gridLocation: GridLocation) => grid(gridLocation) - normals(gridLocation)
  }
}

