/*
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
 *
 */

package geotrellis.raster.über

import geotrellis.raster.testkit.{RasterMatchers, TileBuilders}
import geotrellis.raster.über.JVMArrayTile._
import org.scalatest.{FunSpec, Matchers}
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

/**
 *
 * @author sfitch 
 * @since 1/6/18
 */
class ÜberTileSpec extends FunSpec
  with Matchers
  with RasterMatchers
  with TileBuilders {

  describe("JVM array tile semantics") {
    describe("int tile") {
      val builder = TileBuilder[IntJVMArrayTile]
      val zero = implicitly[AdditiveMonoid[IntJVMArrayTile]].zero
      val t1 = builder.construct(3, 3, createConsecutiveTile(3).toArray)
      val t2 = builder.construct(3, 3, createOnesTile(3).toArray)

      it("has builder") {
        builder.empty.cells.length should === (0)

        val tile = builder.construct(3, 3, createConsecutiveTile(3).toArray())
        assert(tile.cols == 3)
        assert(tile.rows == 3)
        assert(tile.cells.length == 9)
      }

      it("has additive identity") {
        assert(t1 + zero == t1)
        assert(t2 + zero == t2)
        assert(zero + t1 == t1)
        assert(zero + t2 == t2)
      }

      it("has equality") {
        val teq = implicitly[Order[IntJVMArrayTile]]
        assert(teq.eqv(t1, t1))
        assert(teq.eqv(t2, t2))
        assert(t1 == t1)
        assert(t2 == t2)
      }

      it("has order") {
        assert(t1 > t2)
        assert(t1 >= t2)
        assert(t2 < t1)
        assert(t2 <= t1)
        assert(t1 != t2)
        assert(t2 != t1)
      }

      it("supports addition, subtraction, negation") {
        val r0 = -t1
        val r1 = t1 + t2
        val r2 = t1 - t2

        assert(r0 == -t1)
        assert(r1 == t1 + t2)
        assert(r2 == t1 - t2)
        assert(r0 + r1 + r2 == t1)
      }

      it("supports scalar multiplication") {
        assert(t1 :* 1 == t1)
        assert(2 *: t1 == t1 + t1)
      }

      it("support tile multiplication") {
        assert(t2 * t2 == t2)
        assert(t1 * t2 == t1)
        val two = t2 + t2
        assert(t1 * two == t1 + t1)
        assert(t2 ** 4 == t2)
      }
    }

    describe("ubyte tile") {
      val builder = TileBuilder[UByteJVMArrayTile]
      val zero = implicitly[AdditiveMonoid[UByteJVMArrayTile]].zero
      val t1 = builder.construct(3, 3, createConsecutiveTile(3).toArray().map(UByte.apply))
      val t2 = builder.construct(3, 3, createOnesTile(3).toArray().map(UByte.apply))

      it("has builder") {
        builder.empty.cells.length should === (0)

        val tile = builder.construct(3, 3, createConsecutiveTile(3).toArray().map(UByte.apply))
        assert(tile.cols == 3)
        assert(tile.rows == 3)
        assert(tile.cells.length == 9)
      }

      it("has additive identity") {
        assert(t1 + zero == t1)
        assert(t2 + zero == t2)
        assert(zero + t1 == t1)
        assert(zero + t2 == t2)
      }

      it("has equality") {
        val teq = implicitly[Order[UByteJVMArrayTile]]
        assert(teq.eqv(t1, t1))
        assert(teq.eqv(t2, t2))
        assert(t1 == t1)
        assert(t2 == t2)
      }

      it("has order") {
        assert(t1 > t2)
        assert(t1 >= t2)
        assert(t2 < t1)
        assert(t2 <= t1)
        assert(t1 != t2)
        assert(t2 != t1)
      }

      it("supports addition, subtraction") {
        val r1 = t1 + t2
        val r2 = t1 - t2

        assert(r1 == t1 + t2)
        assert(r2 == t1 - t2)
        assert(r1 - r2 == t2 + t2)
      }

      it("supports scalar multiplication") {
        assert(t1 :* 1 == t1)
        assert(2 *: t1 == t1 + t1)
      }

      it("support tile multiplication") {
        assert(t2 * t2 == t2)
        assert(t1 * t2 == t1)
        val two = t2 + t2
        assert(t1 * two == t1 + t1)
        assert(t2 ** 4 == t2)
      }
    }
  }
}
