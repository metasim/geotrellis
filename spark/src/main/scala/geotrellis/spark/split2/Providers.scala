package geotrellis.spark.split2

import geotrellis.raster.Grid
import geotrellis.raster.split.SplitMethods
import geotrellis.util.Component
import geotrellis.vector.ProjectedExtent

/**
 *
 * @author sfitch 
 * @since 7/10/17
 */
object Providers {

  type ProjectedExtentProvider[T] = Component[T, ProjectedExtent]
  type SplitMethodsProvider[T <: Grid] = {
    type Get[A] = (A ⇒ SplitMethods[T])
  }

}
