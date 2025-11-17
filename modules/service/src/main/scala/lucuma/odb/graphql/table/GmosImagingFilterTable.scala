// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.codec.numeric.int4

trait GmosImagingFilterTable[F[_]] extends BaseMapping[F]:

  object GmosNorthImagingFilterTable extends TableDef("t_gmos_north_imaging_filter"):

    val ObservationId: ColumnRef      = col("c_observation_id",        observation_id)
    val Filter: ColumnRef             = col("c_filter",                gmos_north_filter)
    val ExposureTimeModeId: ColumnRef = col("c_exposure_time_mode_id", int4)
    val Version: ColumnRef            = col("c_version",               observing_mode_row_version)

  object GmosSouthImagingFilterTable extends TableDef("t_gmos_south_imaging_filter"):

    val ObservationId: ColumnRef      = col("c_observation_id",        observation_id)
    val Filter: ColumnRef             = col("c_filter",                gmos_south_filter)
    val ExposureTimeModeId: ColumnRef = col("c_exposure_time_mode_id", int4)
    val Version: ColumnRef            = col("c_version",               observing_mode_row_version)
