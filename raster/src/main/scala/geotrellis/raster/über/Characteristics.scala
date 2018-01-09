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


trait MappableTile[C, T <: MappableTile[C, T]] extends ÜberTile[C, T] {
  def map(f: C ⇒ C): T
  def reduceOption(op: (C, C) ⇒ C): Option[C]
  def zip(other: ⇒ T)(f: (C, C) ⇒ C): T
}

trait AddressableCellTile[C] { self: ÜberTile[_, _] ⇒
  def get(col: Int, row: Int): C
}

trait LinearlyAddressedTile[C, T <: ÜberTile[C, T]] extends  ÜberTile[C, T] with AddressableCellTile[C] {
  def cellIndex(col: Int, row: Int): Int
  def indexCell(index: Int): (Int, Int)
  def get(index: Int): C
  def get(col: Int, row: Int): C = get(cellIndex(col, row))
}

trait ColumnMajorTile { self: ÜberTile[_, _] ⇒
  def cellIndex(col: Int, row: Int): Int = row * cols + col
  def indexCell(index: Int) = (index % cols, index / cols)
}
