package geotrellis

import geotrellis.raster._
import geotrellis.raster.io.geotiff.SinglebandGeoTiff

/**
 * Module utilities. Originally from geotrellis-benchmark
 */
package object bench {
  private[geotrellis] val rnd =  new scala.util.Random(42)

  /** Sugar for building arrays using a per-cell init function */
  def init[A: Manifest](size: Int)(init: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  def noDataCount(t: Tile) = {
    var count: Long = 0
    t.dualForeach(
      z ⇒ if(isNoData(z)) count = count + 1
    )(
      z ⇒ if(isNoData(z)) count = count + 1
    )
    count
  }

  def readSinglebandGeoTiff(name: String) =
    SinglebandGeoTiff(getClass.getResource("/" + name).getPath)

  /** Construct a tile of given size and cell type populated with random values. */
  def randomTile(cols: Int, rows: Int, cellTypeName: String): Tile = {
    val cellType = CellType.fromName(cellTypeName)
    val tile = ArrayTile.alloc(cellType, cols, rows)

    // Initialize tile with some initial random values
    var result = tile.dualMap(_ ⇒ rnd.nextInt())(_ ⇒ rnd.nextGaussian())

    // Due to cell width narrowing and custom NoData values, we can end up randomly creating
    // NoData values. While perhaps inefficient, the safest way to ensure a tile with no-NoData values
    // with the current CellType API (GT 1.1), while still generating random data is to
    // iteratively pass through all the cells and replace NoData values as we find them.
    do {
      result = result.dualMap(
        z ⇒ if (isNoData(z)) rnd.nextInt() else z
      ) (
        z ⇒ if (isNoData(z)) rnd.nextGaussian() else z
      )
    } while (noDataCount(result) > 0)
    result
  }
}
