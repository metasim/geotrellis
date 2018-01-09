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

import geotrellis.raster.Grid

/**
 * @tparam C cell word size, i.e. the primitive type (e.g. Int, Double, Byte, etc.)
 * @tparam T subtype bound
 * @author sfitch
 * @since 1/5/18
 */
trait ÜberTile[C, T <: ÜberTile[C, T]] extends Grid {
  type StorageType
  type CellType = C
  def cells: T#StorageType
}

object ÜberTile {
  type Aux[C, T <: ÜberTile[C, T], S] = ÜberTile[C, T] { type StorageType = S }
}

