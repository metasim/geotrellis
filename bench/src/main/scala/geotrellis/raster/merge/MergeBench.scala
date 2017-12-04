/*
 *
 * Copyright 2017 Astraea, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geotrellis.raster.merge

import geotrellis.bench._
import geotrellis.raster._
import geotrellis.raster.resample.NearestNeighbor
import geotrellis.vector.Extent
import org.openjdk.jmh.annotations.{Mode ⇒ JMHMode, _}
import spire.syntax.cfor.cfor

@BenchmarkMode(Array(JMHMode.AverageTime))
@State(Scope.Thread)
class MergeBench {

  @Param(Array("uint8", "float64"))
  var cellTypeName: String = _

  @Param(Array("512"))
  var tileSize: Int = _

  @Param(Array("8"))
  var splits: Int = _

  val extent = Extent(-180, -90, 180, 90)

  var source: Raster[Tile] = _

  var rasters: Array[Raster[Tile]] = _

  var resultTile: Tile = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    source = Raster(randomTile(tileSize, tileSize, cellTypeName), extent)
    val shuffled = rnd.shuffle(source.split(splits, splits).toSeq)
    rasters = shuffled.toArray
  }

  @Setup(Level.Iteration)
  def clearResult(): Unit = {
    resultTile = source.tile.prototype(tileSize, tileSize)
  }

  @Benchmark
  def merge: Tile = {
    var result: Tile = resultTile
    cfor(0)(_ < rasters.length, _ + 1) { i ⇒
      val nextRaster = rasters(i)
      val nextTile = result.merge(
        extent, nextRaster.extent, nextRaster.tile, NearestNeighbor
      )
      result = nextTile
    }
    result
  }
}

//object MergeBench {
//
//  /** Main routine for running code in separate profiler. */
//  def main(args: Array[String]): Unit = {
//    val iterations = 1 << 12
//    println("Initializing run...")
//    val bench = new MergeBench
//    bench.cellTypeName = "float64"
//    bench.tileSize = 2048
//    bench.splits = 8
//    bench.setup()
//    println(s"Running for $iterations iterations.")
//    cfor(0)(_ < iterations, _ + 1) { _ ⇒
//      bench.merge
//    }
//    println("Done.")
//  }
//}
