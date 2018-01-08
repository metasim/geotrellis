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

import java.nio.{Buffer, IntBuffer}

import geotrellis.raster.{CellType, IntCellType, Tile}

/**
 *
 * @author sfitch 
 * @since 1/6/18
 */
trait NIOBufferTile[C, T <: NIOBufferTile[C, T]] extends ÜberTile[C, T] {
  def zip(other: ⇒ T)(f: (C, C) ⇒ C) = ???
  def map(f: C ⇒ C) = ???
}

object NIOBufferTile {

  case class IntBufferTile(cols: Int, rows: Int, cells: IntBuffer) extends NIOBufferTile[Int, IntBufferTile] {
    type StorageType = IntBuffer
  }

  object IntBufferTile {
    /** Convenience constructor for converting from existing Tile. */
    def apply(t: Tile) = new IntBufferTile(t.cols, t.rows,
      IntBuffer.wrap(t.toArrayTile().toArray)
    )
  }
}
