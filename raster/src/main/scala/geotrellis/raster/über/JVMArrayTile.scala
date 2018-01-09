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
abstract class JVMArrayTile[C: ClassTag, T <: JVMArrayTile[C, T]: ClassTag] extends MappableTile[C, T] with ColumnMajorTile[C] { self: Product ⇒

  type StorageType = Array[C]

  def get(index: Int) = cells(index)

  def map(f: C ⇒ C): T = {
    val buf = self.cells.clone()
    cfor(0)(_ < buf.length, _ + 1) { i ⇒
      buf(i) = f(buf(i))
    }
    jvmTileHasBuilder[C, T].construct(cols, rows, buf)
  }

  def zip(other: ⇒ T)(f: (C, C) ⇒ C): T = {
    val left = self
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
    jvmTileHasBuilder[C, T].construct(cols, rows, buf)
  }

  override def equals(that: scala.Any) = self.canEqual(that) && (that match {
    case t: JVMArrayTile[_, _] ⇒
      self.cols == t.cols && self.rows == t.rows &&
      self.cells.deep == t.cells.deep
    case _ ⇒ false
  })

  override def toString = if(cells.length > 20)
    s"$productPrefix($cols, $rows, ${cells.take(20).mkString(", ")}...)"
  else
    s"$productPrefix($cols, $rows, ${cells.mkString(", ")})"
}

object JVMArrayTile {

  trait Implicits {
    class JVMTileBuilder[C: ClassTag, T <: JVMArrayTile[C, T]: ClassTag] extends TileBuilder[T] {
      val ctor = classTag[T].runtimeClass.asInstanceOf[Class[T]].getConstructor(
        Integer.TYPE, Integer.TYPE, classTag[C].wrap.runtimeClass
      )
      def empty = ctor.newInstance(Int.box(0), Int.box(0), Array.empty[C])
      def construct(cols: Int, rows: Int, cells: Array[C]) = ctor.newInstance(Int.box(cols), Int.box(rows), cells)
    }

    implicit def jvmTileHasBuilder[C: ClassTag, T <: JVMArrayTile[C, T]: ClassTag]: TileBuilder[T] = new JVMTileBuilder[C, T]
    def jvmTileHasModule[C: Ring: ClassTag, T <: JVMArrayTile[C, T]: TileBuilder: ClassTag: TypeTag]: Module[T, C] =
      new ÜberAlgebra.MappableTileModule[C, T]
    implicit def jvmTileHasOrder[C: Order: ClassTag, T <: JVMArrayTile[C, T]]: Order[T] =
      new ÜberAlgebra.LinearlyAddressedTileOrder[C, T]


  }

  case class IntJVMArrayTile(cols: Int, rows: Int, cells: Array[Int]) extends JVMArrayTile[Int, IntJVMArrayTile] {
    def construct(col: Int, row: Int, cells: Array[Int]) = IntJVMArrayTile(col, row, cells)
  }

  object IntJVMArrayTile {
    /** Convenience constructor for converting from existing Tile. */
    def apply(t: Tile) = new IntJVMArrayTile(t.cols, t.rows, t.toArrayTile().toArray)
    implicit val intTileHasBuilder = jvmTileHasBuilder[Int, IntJVMArrayTile]
    implicit val intTileHasModule = jvmTileHasModule[Int, IntJVMArrayTile]
    implicit val intTileHasOrder = jvmTileHasOrder[Int, IntJVMArrayTile]
  }
}



