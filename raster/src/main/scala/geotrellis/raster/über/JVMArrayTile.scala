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

import java.util

import geotrellis.raster.Tile
import geotrellis.raster.über.JVMArrayTile.JVMTileBuilder
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.std._

import scala.reflect._
import scala.reflect.runtime.universe._

/**
 *
 * @author sfitch 
 * @since 1/6/18
 */
abstract class JVMArrayTile[C: ClassTag, T <: JVMArrayTile[C, T]: ClassTag] extends MappableTile[C, T] with ColumnMajorTile[C] with Product {

  type StorageType = Array[C]

  lazy val builder = new JVMTileBuilder[C, T]

  def get(index: Int) = cells(index)

  def map(f: C ⇒ C): T = {
    val buf = cells.clone()
    cfor(0)(_ < buf.length, _ + 1) { i ⇒
      buf(i) = f(buf(i))
    }
    builder.construct(cols, rows, buf)
  }

  def reduceOption[C1 >: C](op: (C1, C1) ⇒ C1): Option[C1] = {
    if(size == 0) None
    else cells.reduceOption(op)
  }

  def zip(other: ⇒ T)(f: (C, C) ⇒ C): T = {
    val left = this
    val right = other
    val cols = min(left.cols, right.cols)
    val rows = min(left.rows, right.rows)
    val buf = Array.ofDim[C](cols * rows)

    val lcells = left.cells
    val rcells = right.cells

    cfor(0)(_ < rows, _ + 1) { r ⇒
      cfor(0)(_ < cols, _ + 1) { c ⇒
        val i = cellIndex(c, r)
        buf(i) = f(lcells(i), rcells(i))
      }
    }
    builder.construct(cols, rows, buf)
  }

  override def equals(that: scala.Any) = canEqual(that) && (that match {
    case t: JVMArrayTile[_, _] ⇒
      cols == t.cols && rows == t.rows &&
      cells.deep == t.cells.deep
    case _ ⇒ false
  })

  override def toString =
    s"$productPrefix($cols, $rows, ${cells.take(20).mkString(", ")}${if(cells.length > 20) "...)" else ")"}"
}

object JVMArrayTile {
  class JVMTileBuilder[C: ClassTag, T <: JVMArrayTile[C, T]: ClassTag] extends TileBuilder[T] {
    val ctor = classTag[T].runtimeClass.asInstanceOf[Class[T]].getConstructor(
      Integer.TYPE, Integer.TYPE, classTag[C].wrap.runtimeClass
    )
    def empty = ctor.newInstance(Int.box(0), Int.box(0), Array.empty[C])
    def construct(cols: Int, rows: Int, cells: Array[C]) = ctor.newInstance(Int.box(cols), Int.box(rows), cells)
  }

  trait Implicits {

    implicit val intTileHasBuilder = new JVMTileBuilder[Int, IntJVMArrayTile]
    implicit val intTileAlgebra1 = new ÜberAlgebra.SignedMappableTileAlgebra[Int, IntJVMArrayTile]
    implicit val intTileAlgebra2 = new ÜberAlgebra.LinearlyAddressedTileAlgebra[Int, IntJVMArrayTile]

    implicit val ubyteAsAdditiveGroup = new Ring[UByte] {
      def negate(x: UByte) = -x // <--- not sure what the implications of this are....
      def zero = UByte.MinValue
      def one = UByte(1)
      def plus(x: UByte, y: UByte) = x + y
      override def minus(x: UByte, y: UByte) = x - y
      def times(x: UByte, y: UByte) = x * y
    }

    implicit val ubyteTileHasBuilder = new JVMTileBuilder[UByte, UByteJVMArrayTile]
    implicit val ubyteTileAlgebra1 = new ÜberAlgebra.UnsignedMappableTileAlgebra[UByte, UByteJVMArrayTile]
    implicit val ubyteTileAlgebra2 = new ÜberAlgebra.LinearlyAddressedTileAlgebra[UByte, UByteJVMArrayTile]

  }

  case class IntJVMArrayTile(cols: Int, rows: Int, cells: Array[Int]) extends JVMArrayTile[Int, IntJVMArrayTile]

  case class UByteJVMArrayTile(cols: Int, rows: Int, cells: Array[UByte]) extends JVMArrayTile[UByte, UByteJVMArrayTile]
}



