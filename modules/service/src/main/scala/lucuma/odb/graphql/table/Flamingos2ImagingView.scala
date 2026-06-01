// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.codec.all.*

trait Flamingos2ImagingView[F[_]] extends BaseMapping[F]:

  object Flamingos2ImagingFilterTable extends TableDef("t_flamingos_2_imaging_filter"):
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val Filter: ColumnRef        = col("c_filter", flamingos_2_filter)
    val Version: ColumnRef       = col("c_version", observing_mode_row_version)
    val ExposureTimeModeId       = col("c_exposure_time_mode_id", exposure_time_mode_id)
    val Role: ColumnRef          = col("c_role", exposure_time_mode_role)

  object Flamingos2ImagingView extends TableDef("v_flamingos_2_imaging"):

    val ObservationId: ColumnRef      = col("c_observation_id", observation_id)

    val Filters: ColumnRef            = col("c_filters", _flamingos_2_filter)

    val ReadMode: ColumnRef           = col("c_read_mode", flamingos_2_read_mode.opt)
    val ReadModeDefault: ColumnRef    = col("c_read_mode_default", flamingos_2_read_mode)

    val Reads: ColumnRef              = col("c_reads", flamingos_2_reads.opt)
    val ReadsDefault: ColumnRef       = col("c_reads_default", flamingos_2_reads)

    val Decker: ColumnRef             = col("c_decker", flamingos_2_decker.opt)
    val DeckerDefault: ColumnRef      = col("c_decker_default", flamingos_2_decker)

    val ReadoutMode: ColumnRef        = col("c_readout_mode", flamingos_2_readout_mode.opt)
    val ReadoutModeDefault: ColumnRef = col("c_readout_mode_default", flamingos_2_readout_mode)

    val Offsets: ColumnRef            = col("c_offsets", text.opt)
