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

import spire.algebra._

import scala.reflect.ClassTag

/**
 *
 * @author sfitch 
 * @since 1/7/18
 */
object ÜberAlgebra {
  @SerialVersionUID(0L)
  class LinearlyAddressedTileOrder[C, T <: LinearlyAddressedTile[C]](implicit ord: Order[C]) extends Order[T] with Serializable {
    override def eqv(x: T, y: T): Boolean = x.cols == y.cols && x.rows == y.cols && compare(x, y) == 0
    def compare(x: T, y: T): Int = {
      var i = 0
      while (i < x.size && i < y.size) {
        val cmp = x.get(i) compare y.get(i)
        if (cmp != 0) return cmp
        i += 1
      }
      x.size - y.size
    }
  }

  @SerialVersionUID(0L)
  class MappableTileModule[C: ClassTag, T <: MappableTile[C, T]: TileBuilder: ClassTag](
    implicit override val scalar: Ring[C]) extends Module[T, C] with Serializable {
    def zero: T = TileBuilder[T].empty
    override def isZero(a: T)(implicit ev: Eq[T]): Boolean = a == zero
    def negate(x: T): T = x.map(scalar.negate)
    def plus(x: T, y: T): T = x.zip(y)(scalar.plus)
    override def minus(x: T, y: T): T = x.zip(y)(scalar.minus)
    def timesl(r: C, v: T): T = v.map(scalar.times(r, _))
    override def timesr(v: T, r: C): T = v.map(scalar.times(_, r))
  }

  trait Implicits {
    implicit def linearlyAddressedTileOrder[C, T <: LinearlyAddressedTile[C]](implicit ord: Order[C]) = new LinearlyAddressedTileOrder[C, T]
    implicit def mappableTileModule[C: ClassTag, T <: MappableTile[C, T]: TileBuilder: ClassTag](implicit scalar: Ring[C]) = new MappableTileModule[C, T]
  }
}

