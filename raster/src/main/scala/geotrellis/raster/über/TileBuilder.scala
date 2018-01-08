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

/**
 *
 * @author sfitch 
 * @since 1/7/18
 */
trait TileBuilder[T <: ÜberTile[_, T]] {
  def empty: T
  def construct(cols: Int, rows: Int, cells: T#StorageType): T
}
object TileBuilder {
  def apply[T <: ÜberTile[_, T]: TileBuilder] = implicitly[TileBuilder[T]]
}
