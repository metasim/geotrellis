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

import spire.NoImplicit
import spire.algebra._

import scala.reflect.ClassTag

/**
 *
 * @author sfitch 
 * @since 1/7/18
 */
object ÜberAlgebra {

  trait LinearlyAddressedTileHasOrder[C, T <: LinearlyAddressedTile[C]] extends Order[T] {
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

  trait MappableTileIsEq[C, T <: MappableTile[C, T]] extends Eq[T] {
    def scalar: AdditiveGroup[C]
    def eqv(x: T, y: T): Boolean = x.cols == y.cols && x.rows == y.rows && cellsEqual(x,y)
    private def cellsEqual(x: T, y: T): Boolean =
      x.zip(y)(scalar.minus).reduceOption(scalar.plus).getOrElse(0) == 0
  }

  trait MapapbleTileIsAdditiveMonoid[C, T <: MappableTile[C, T]] extends AdditiveMonoid[T] with MappableTileIsEq[C, T] { self ⇒
    def scalar: AdditiveGroup[C]
    def builder: TileBuilder[T]
    def zero = builder.empty
    def plus(x: T, y: T): T = if(isZero(x)(self)) y else if(isZero(y)(self)) x else x.zip(y)(scalar.plus)
    override def isZero(a: T)(implicit ev: Eq[T]): Boolean = a.size == 0
  }

  trait MappableTileIsRng[C, T <: MappableTile[C, T]] extends MapapbleTileIsAdditiveMonoid[C, T] with Rng[T] { self ⇒
    def scalar: Rng[C]
    def builder: TileBuilder[T]
    def times(x: T, y: T): T =  if(isZero(x)(self)) x else if(isZero(y)(self)) y else x.zip(y)(scalar.times)
    def negate(x: T): T = x.map(scalar.negate)
    override protected def prodnAboveOne(a: T, n: Int): T = a.map(scalar.pow(_, n))
  }

  trait MappableTileIsModule[C, T <: MappableTile[C, T]] extends MappableTileIsRng[C, T] with Module[T, C] {
    override def minus(x: T, y: T): T = x.zip(y)(scalar.minus)
    def timesl(r: C, v: T): T = v.map(scalar.times(r, _))
  }

  @SerialVersionUID(0L)
  class SignedMappableTileAlgebra[C: Rng, T <: MappableTile[C, T]: TileBuilder]
    extends MappableTileIsModule[C, T] {
    def builder = TileBuilder[T]
    def scalar = Rng[C]
  }

  @SerialVersionUID(0L)
  class UnsignedMappableTileAlgebra[C: Rng, T <: MappableTile[C, T]: TileBuilder]
    extends MappableTileIsModule[C, T] {
    def builder = TileBuilder[T]
    def scalar = Rng[C]
  }
}

