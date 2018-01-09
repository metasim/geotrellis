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

import java.nio.IntBuffer

import geotrellis.raster.über.NIOBufferTile._
import spire.implicits._
import spire.math._

import scala.reflect._

/**
 *
 * @author sfitch 
 * @since 1/6/18
 */
abstract class NIOBufferTile[C, T <: NIOBufferTile[C, T]: ClassTag]
  extends LinearlyAddressedTile[C, T]
    with MappableTile[C, T] with ColumnMajorTile with Product {

  protected val builder: TileBuilderWithBufferIO[C, T, T#StorageType]

  def map(f: C ⇒ C): T = {
    val buf = builder.alloc(size)
    cfor(0)(_ < size, _ + 1) { i ⇒
      builder.put(buf, i, f(builder.get(cells, i)))
    }
    builder.construct(cols, rows, buf)
  }

  def reduceOption(op: (C, C) ⇒ C): Option[C] = {
    if(size == 0) None
    else {
      var result = builder.get(cells, 0)
      cfor(1)(_ < size, _ + 1) { i ⇒
        result = op(result, builder.get(cells, i))
      }
      Some(result)
    }
  }

  def zip(other: ⇒ T)(f: (C, C) ⇒ C): T = {
    val left: T = this.asInstanceOf[T] // <--- need a type ninja to fix this
    val right = other
    val cols = min(left.cols, right.cols)
    val rows = min(left.rows, right.rows)
    val buf = left.builder.alloc(size)

    val lcells = left.cells
    val rcells = right.cells

    cfor(0)(_ < rows, _ + 1) { r ⇒
      cfor(0)(_ < cols, _ + 1) { c ⇒
        val i = cellIndex(c, r)
        val lc = left.builder.get(lcells, i)
        val rc = right.builder.get(rcells, i)
        left.builder.put(buf, i, f(lc, rc))
      }
    }
    builder.construct(cols, rows, buf)
  }

  override def equals(that: scala.Any) = canEqual(that) && (that match {
    case t: NIOBufferTile[_, _] ⇒
      cols == t.cols && rows == t.rows && cells == t.cells
    case _ ⇒ false
  })

  private def take(num: Int): Seq[C] = {
    for {
      i ← 0 until min(num, size)
      c = builder.get(cells, i)
    } yield c
  }

  override def toString = {
    val bufType = cells.getClass.getSimpleName
    s"$productPrefix($cols, $rows, $bufType(${take(20).mkString(", ")}${if(size > 20) "...)" else "))"}"
  }
}

object NIOBufferTile {
  trait TileBuilderWithBufferIO[C, T <: NIOBufferTile[C, T], B <: T#StorageType] extends TileBuilder[T] {
    def alloc(size: Int): B
    def get(buf: B, index: Int): C
    def put(buf: B, index: Int, value: C): Unit
  }

  case class IntBufferTile(cols: Int, rows: Int, cells: IntBuffer) extends NIOBufferTile[Int, IntBufferTile] {
    type StorageType = IntBuffer
    protected val builder = IntBufferTileBuilder

    def get(index: Int): Int = builder.get(cells, index)
  }

  trait Implicits {
    implicit object IntBufferTileBuilder extends TileBuilderWithBufferIO[Int, IntBufferTile, IntBuffer] {
      def alloc(size: Int): IntBuffer = IntBuffer.allocate(size)
      def get(buf: IntBuffer, index: Int): Int = buf.get(index)
      def put(buf: IntBuffer, index: Int, value: Int): Unit = buf.put(index, value)
      def empty: IntBufferTile = construct(0, 0, alloc(0))
      def construct(cols: Int, rows: Int, cells: IntBuffer): IntBufferTile = IntBufferTile(cols, rows, cells)
    }
    implicit val intBufferTileHasBuilder: TileBuilder[IntBufferTile] = IntBufferTileBuilder
    implicit val intBufferTileAlgebra1 = new ÜberAlgebra.SignedMappableTileAlgebra[Int, IntBufferTile]
    implicit val intBufferTileAlgebra2 = new ÜberAlgebra.LinearlyAddressedTileAlgebra[Int, IntBufferTile]
  }
}
