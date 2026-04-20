// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*

trait GhostDynamicTable[F[_]] extends BaseMapping[F]:

  object GhostDynamicTable extends TableDef("t_ghost_dynamic"):
    val Id: ColumnRef         = col("c_step_id",    step_id)
    val Instrument: ColumnRef = col("c_instrument", instrument)

    abstract class Channel(val name: String):
      val ExposureTime: ColumnRef   = col(s"c_${name}_exposure_time",  time_span)
      def ExposureCount: ColumnRef  = col(s"c_${name}_exposure_count", int4_pos)
      def Binning: ColumnRef        = col(s"c_${name}_binning",        ghost_binning)
      def ReadMode: ColumnRef       = col(s"c_${name}_read_mode",      ghost_read_mode)

    object Blue extends Channel("blue")
    object Red extends Channel("red")

    object FiberAgitator:
      val Ifu1: ColumnRef = col("c_ifu1_fiber_agitator", ghost_ifu1_fiber_agitator)
      val Ifu2: ColumnRef = col("c_ifu2_fiber_agitator", ghost_ifu2_fiber_agitator)