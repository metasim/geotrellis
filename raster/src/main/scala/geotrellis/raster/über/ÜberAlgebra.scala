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

  trait LinearlyAddressedTileHasOrder[C, T <: LinearlyAddressedTile[C]] extends Order[T] with Serializable {
    def scalar: Order[C]
    override def eqv(x: T, y: T): Boolean = x.cols == y.cols && x.rows == y.cols && compare(x, y) == 0
    def compare(x: T, y: T): Int = {
      var i = 0
      while (i < x.size && i < y.size) {
        val cmp = scalar.compare(x.get(i), y.get(i))
        if (cmp != 0) return cmp
        i += 1
      }
      // fallout test
      x.size - y.size
    }
  }

  @SerialVersionUID(0L)
  class LinearlyAddressedTileAlgebra[C: Order, T <: LinearlyAddressedTile[C]] extends LinearlyAddressedTileHasOrder[C, T] {
    def scalar = Order[C]
  }

  trait MappableTileIsRng[C, T <: MappableTile[C, T]] extends Rng[T] with Serializable {
    def scalar: Rng[C]
    def builder: TileBuilder[T]
    def zero: T = builder.empty
    def times(x: T, y: T): T =  x.zip(y)(scalar.times)
    def negate(x: T): T = x.map(scalar.negate)
    def plus(x: T, y: T): T = x.zip(y)(scalar.plus)
    override protected def prodnAboveOne(a: T, n: Int): T = a.map(scalar.pow(_, n))
  }

  trait MappableTileIsModule[C, T <: MappableTile[C, T]] extends MappableTileIsRng[C, T] with Module[T, C] {
    override def minus(x: T, y: T): T = x.zip(y)(scalar.minus)
    def timesl(r: C, v: T): T = v.map(scalar.times(r, _))
  }

  @SerialVersionUID(0L)
  class SignedMappableTileAlgebra[C: Rng, T <: MappableTile[C, T]: TileBuilder: ClassTag]
    extends MappableTileIsModule[C, T] {
    def builder = TileBuilder[T]
    def scalar = Rng[C]
  }

  trait Implicits {
    implicit def mappableTileModule[C: Rng, T <: MappableTile[C, T]: TileBuilder: ClassTag] = new SignedMappableTileAlgebra[C, T]
    implicit def linearlyAddressedTileOrder[C: Order, T <: LinearlyAddressedTile[C]] = new LinearlyAddressedTileAlgebra[C, T]

  }
}

