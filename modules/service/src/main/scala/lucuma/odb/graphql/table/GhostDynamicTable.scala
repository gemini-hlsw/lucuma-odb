// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*
import org.tpolecat.typename.TypeName
import skunk.codec.numeric.int8

trait GhostDynamicTable[F[_]] extends BaseMapping[F]:

  class GhostDynamicTable[I](
    name:   String,
    idName: String,
    id:     skunk.Codec[I]
  )(using TypeName[I]) extends TableDef(name):
    val Id: ColumnRef         = col(idName,         id)
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

  object GhostDynamicTable extends GhostDynamicTable(
    "t_ghost_dynamic",
    "c_step_id",
    step_id
  )

  object GhostCalibrationSetupTable extends GhostDynamicTable(
    "t_ghost_daytime_calibration_setup",
    "c_set_id",
    int8
  )
