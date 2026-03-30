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

    object Blue:
      val ExposureTime: ColumnRef  = col("c_blue_exposure_time",  time_span)
      val ExposureCount: ColumnRef = col("c_blue_exposure_count", int4_pos)
      val Binning: ColumnRef       = col("c_blue_binning",        ghost_binning)
      val ReadMode: ColumnRef      = col("c_blue_read_mode",      ghost_read_mode)

    object Red:
      val ExposureTime: ColumnRef  = col("c_red_exposure_time",  time_span)
      val ExposureCount: ColumnRef = col("c_red_exposure_count", int4_pos)
      val Binning: ColumnRef       = col("c_red_binning",        ghost_binning)
      val ReadMode: ColumnRef      = col("c_red_read_mode",      ghost_read_mode)

    object FiberAgitator:
      val Ifu1: ColumnRef = col("c_ifu1_fiber_agitator", ghost_ifu1_fiber_agitator)
      val Ifu2: ColumnRef = col("c_ifu2_fiber_agitator", ghost_ifu2_fiber_agitator)