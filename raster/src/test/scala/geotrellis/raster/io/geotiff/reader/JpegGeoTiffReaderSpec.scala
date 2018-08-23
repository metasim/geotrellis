/*
 * Copyright 2016 Azavea
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

package geotrellis.raster.io.geotiff.reader

import geotrellis.raster._
import geotrellis.raster.io.geotiff._
import geotrellis.raster.testkit._
import org.scalatest._

class JpegGeoTiffReaderSpec extends FunSpec
    with RasterMatchers
    with GeoTiffTestUtils {


  describe("Reading a geotiff with JPEG compression") {
    // TO USE: Download
    //  https://oin-hotosm.s3.amazonaws.com/5b437bcc2b6a08001185f94c/0/5b437bcc2b6a08001185f94d.tif
    // and call it "jpeg-test.tif" in the  raster/data/geotiff-test-files folder
    val (baseJpg, extent, crs) = {
      val gt = GeoTiffReader.readMultiband(geoTiffPath(s"jpeg-test.tif"))
      (gt.tile.toArrayTile(), gt.raster.extent, gt.crs)
    }

    // run gdal_translate -co compression=deflate jpeg-test.tif jpeg-test-deflate.tif to create our expected.
    lazy val deflate = {
      val gt = GeoTiffReader.readMultiband(geoTiffPath(s"jpeg-test-deflate.tif"))
      gt.tile.toArrayTile()
    }

    lazy val rgbJpg = {
      // run gdal_translate jpeg-test.tif -co PHOTOMETRIC=RGB -co COMPRESS=JPEG jpeg-test-rgb.tif to create expected.
      val gt = GeoTiffReader.readMultiband(geoTiffPath(s"jpeg-test-rgb.tif"))
      gt.tile.toArrayTile()
    }

    it("should read jpeg compressed GeoTiff encoded YCbCr") {
      GeoTiff(Raster(baseJpg, extent), crs).copy(
        options=GeoTiffOptions.DEFAULT//.copy(colorSpace=6)
      ).write("jpeg-test-written-base.tif")

      assertEqual(baseJpg, deflate)
    }

    it("should read jpeg compressed GeoTiff encoded RGB") {
//      GeoTiff(Raster(rgbJpg, extent), crs).copy(
//        options=GeoTiffOptions.DEFAULT//.copy(colorSpace=6)
//      ).write("jpeg-test-written-rgb.tif")

//      val diff = Abs(Subtract(rgbJpg.band(0), deflate.band(0)))
//      diff.statisticsDouble.foreach(println)
//
//      GeoTiff(Raster(diff, extent), crs).copy(
//        options=GeoTiffOptions.DEFAULT//.copy(colorSpace=6)
//      ).write("jpeg-test-band-0-delta2.tif")

      assertEqual(rgbJpg, deflate)
    }
  }
}
