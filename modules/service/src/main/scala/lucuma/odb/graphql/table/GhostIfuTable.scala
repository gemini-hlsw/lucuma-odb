// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*

trait GhostIfuTable[F[_]] extends BaseMapping[F]:

  object GhostIfuTable extends TableDef("t_ghost_ifu"):
    val ObservationId: ColumnRef     = col("c_observation_id",      observation_id)
    val ProgramId:     ColumnRef     = col("c_program_id",          program_id)
    val Instrument:    ColumnRef     = col("c_instrument",          instrument)
    val ObservingModeType: ColumnRef = col("c_observing_mode_type", observing_mode_type)

    val ResolutionMode: ColumnRef    = col("c_resolution_mode",     ghost_resolution_mode)

    case class DetectorTable(name: String):
      val ExposureTimeModeId: ColumnRef = col(s"c_${name}_exposure_time_mode_id", exposure_time_mode_id)
      val Binning: ColumnRef            = col(s"c_${name}_binning",               ghost_binning.opt)
      val BinningDefault: ColumnRef     = col(s"c_${name}_binning_default",       ghost_binning)
      val ReadMode: ColumnRef           = col(s"c_${name}_read_mode",             ghost_read_mode.opt)
      val ReadModeDefault: ColumnRef    = col(s"c_${name}_read_mode_default",     ghost_read_mode)

    val Red: DetectorTable  = DetectorTable("red")
    val Blue: DetectorTable = DetectorTable("blue")

    val Ifu1FiberAgitator: ColumnRef = col("c_ifu1_fiber_agitator", ghost_ifu1_fiber_agitator.opt)
    val Ifu2FiberAgitator: ColumnRef = col("c_ifu2_fiber_agitator", ghost_ifu2_fiber_agitator.opt)