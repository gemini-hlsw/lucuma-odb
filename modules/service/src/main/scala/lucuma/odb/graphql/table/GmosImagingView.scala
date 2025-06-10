// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.circe.codec.json.*
import skunk.codec.all.*

trait GmosImagingView[F[_]] extends BaseMapping[F]:
  class CommonImagingColumns(implicit val tableName: TableName):

    val ObservationId: ColumnRef            = col("c_observation_id", observation_id)

    val Bin: ColumnRef                      = col("c_bin", gmos_binning.opt)
    val BinDefault: ColumnRef               = col("c_bin_default", gmos_binning)
    val AmpReadMode: ColumnRef              = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val AmpGain: ColumnRef                  = col("c_amp_gain", gmos_amp_gain.opt)
    val Roi: ColumnRef                      = col("c_roi", gmos_roi.opt)

  object GmosNorthImagingView extends TableDef("v_gmos_north_imaging"):

    val ObservationId: ColumnRef      = col("c_observation_id", observation_id)

    val Filters: ColumnRef            = col("c_filters", _gmos_north_filter)

    val Common: CommonImagingColumns     = new CommonImagingColumns()

    object GmosNorthImagingFiltersView extends TableDef("v_gmos_north_imaging_filter"):

      val ObservationId: ColumnRef      = col("c_observation_id", observation_id)

      val Filter: ColumnRef            = col("c_filter", gmos_north_filter)

  object GmosSouthImagingView extends TableDef("v_gmos_south_imaging"):

    val ObservationId: ColumnRef      = col("c_observation_id", observation_id)

    val Filters: ColumnRef            = col("c_filters", _gmos_south_filter)
    val Common: CommonImagingColumns     = new CommonImagingColumns()
